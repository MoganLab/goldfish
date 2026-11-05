;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai api google-generative-ai core)
  (import (scheme base)
    (scheme char)
    (liii list)
    (liii string)
    (liii json)
    (liii error)
    (liii pi ai message)
    (liii pi ai event)
  ) ;import
  (export gemini-map-finish-reason gemini-map-usage
    gemini-normalize-tool-call-id gemini-generate-tool-call-id
    gemini-build-contents gemini-build-request-body
  ) ;export
  (begin

    (define (gemini-map-finish-reason finish-reason has-function-call?)
      (cond ((or (equal? finish-reason "STOP") (eq? finish-reason 'STOP))
             (if has-function-call? "toolUse" "stop")
            ) ;
            ((or (equal? finish-reason "MAX_TOKENS") (eq? finish-reason 'MAX_TOKENS))
             "length"
            ) ;
            (else "error")
      ) ;cond
    ) ;define

    (define (%usage-field usage-meta key sym-key)
      (if (not (pair? usage-meta))
        0
        (let ((pair (or (assoc key usage-meta) (assoc sym-key usage-meta))))
          (if pair (cdr pair) 0)
        ) ;let
      ) ;if
    ) ;define

    (define (gemini-map-usage usage-meta)
      (let* ((prompt-tokens (%usage-field usage-meta "promptTokenCount" 'promptTokenCount))
             (cand-tokens (%usage-field usage-meta "candidatesTokenCount" 'candidatesTokenCount)
             ) ;cand-tokens
             (thought-tokens (%usage-field usage-meta "thoughtsTokenCount" 'thoughtsTokenCount)
             ) ;thought-tokens
             (cached-tokens (%usage-field usage-meta "cachedContentTokenCount" 'cachedContentTokenCount)
             ) ;cached-tokens
             (total-tokens (%usage-field usage-meta "totalTokenCount" 'totalTokenCount))
             (in-tokens (- prompt-tokens cached-tokens))
             (out-tokens (+ cand-tokens thought-tokens))
            ) ;
        (list (cons 'input in-tokens)
          (cons 'output out-tokens)
          (cons 'cacheRead cached-tokens)
          (cons 'cacheWrite 0)
          (cons 'totalTokens total-tokens)
        ) ;list
      ) ;let*
    ) ;define

    (define (%map-char c)
      (if (or (char-alphabetic? c) (char-numeric? c) (char=? c #\_) (char=? c #\-))
        c
        #\_
      ) ;if
    ) ;define

    (define (gemini-normalize-tool-call-id id)
      (if (not (string? id))
        "tool_call_0"
        (let ((res (list->string (map %map-char (string->list id)))))
          (if (> (string-length res) 64) (substring res 0 64) res)
        ) ;let
      ) ;if
    ) ;define

    (define* (gemini-generate-tool-call-id (name "call") (timestamp 0) (counter 1))
      (gemini-normalize-tool-call-id (string-append name "_" (number->string timestamp) "_" (number->string counter))
      ) ;gemini-normalize-tool-call-id
    ) ;define*

    (define* (%val key obj (default-val #f))
      (let ((p
              (or (assoc key obj)
                (assoc (if (symbol? key) (symbol->string key) (string->symbol key)) obj)
              ) ;or
            ) ;p
           ) ;
        (if (pair? p) (cdr p) default-val)
      ) ;let
    ) ;define*

    ;; thought signature 须为 base64（TYPE_BYTES）：长度 4 倍数且仅含
    ;; [A-Za-z0-9+/] 与至多两个末尾 =（对齐上游 isValidThoughtSignature）
    (define (gemini-valid-thought-signature? sig)
      (and (string? sig)
        (not (string-null? sig))
        (zero? (modulo (string-length sig) 4))
        (let loop ((i 0))
          (cond ((>= i (string-length sig)) #t)
                (else
                  (let ((c (string-ref sig i)))
                    (cond ((or (char-alphabetic? c) (char-numeric? c) (char=? c #\+) (char=? c #\/))
                           (loop (+ i 1))
                          ) ;
                          ((char=? c #\=)
                           ;; = 只允许出现在最后两位
                           (and (>= i (- (string-length sig) 2))
                             (loop (+ i 1))
                           ) ;and
                          ) ;
                          (else #f)
                    ) ;cond
                  ) ;let
                ) ;else
          ) ;cond
        ) ;let
      ) ;and
    ) ;define

    ;; 同 provider+model 且签名合法时才回放（对齐上游 resolveThoughtSignature）
    (define (%resolve-thought-signature same-model? sig)
      (if (and same-model? (gemini-valid-thought-signature? sig)) sig #f)
    ) ;define

    (define (%assistant-block->part blk same-model?)
      (let ((btype (%val 'type blk "")))
        (cond
         ((equal? btype "text")
          (let* ((t (%val 'text blk ""))
                 (sig (%resolve-thought-signature same-model?
                        (%val 'textSignature blk (%val 'text-signature blk #f))
                      ) ;%resolve-thought-signature
                 ) ;sig
                ) ;
            ;; 空文本但带签名的块保留（Gemini 3 要求签名回显）
            (if (or (not (string? t)) (and (string-null? (string-trim-both t)) (not sig)))
              #f
              (if sig
                (list (cons "thoughtSignature" sig) (cons "text" t))
                (list (cons "text" t))
              ) ;if
            ) ;if
          ) ;let*
         ) ;
         ((equal? btype "thinking")
          (let* ((th (%val 'thinking blk ""))
                 (sig (%resolve-thought-signature same-model?
                        (%val 'thinkingSignature blk (%val 'thinking-signature blk #f))
                      ) ;%resolve-thought-signature
                 ) ;sig
                 (empty? (or (not (string? th)) (string-null? (string-trim-both th))))
                ) ;
            (cond
             (same-model?
              (if (and empty? (not sig))
                #f
                (let ((base (list (cons "thought" #t) (cons "text" th))))
                  (if sig (cons (cons "thoughtSignature" sig) base) base)
                ) ;let
              ) ;if
             ) ;
             ;; 跨模型：thinking 降级为纯文本，不回传签名
             (empty? #f)
             (else (list (cons "text" th)))
            ) ;cond
          ) ;let*
         ) ;
         ((equal? btype "toolCall")
          (let* ((ts (%resolve-thought-signature same-model? (%val 'thoughtSignature blk #f)))
                 (fc-obj (list (cons "name" (%val 'name blk ""))
                           (cons "args" (%val 'arguments blk '()))
                           (cons "id" (%val 'id blk ""))
                         ) ;list
                 ) ;fc-obj
                 (part (list (cons "functionCall" fc-obj)))
                ) ;
            (if ts (cons (cons "thoughtSignature" ts) part) part)
          ) ;let*
         ) ;
         (else #f)
        ) ;cond
      ) ;let
    ) ;define

    (define (%is-image-block? b)
      (or (pi-image-block? b) (equal? (%val 'type b "") "image"))
    ) ;define

    (define (%image-block->gemini-part b)
      (let ((mime (if (pi-image-block? b)
                    (pi-image-block-mime-type b)
                    (%val 'mimeType b (%val 'media_type b "image/png"))
                  ) ;if
            ) ;mime
            (data (if (pi-image-block? b) (pi-image-block-data b) (%val 'data b "")))
           ) ;
        (list (cons "inlineData" (list (cons "mimeType" mime) (cons "data" data))))
      ) ;let
    ) ;define

    (define (%extract-user-text-content content)
      (cond ((string? content) content)
            ((or (vector? content) (pair? content))
             (let ((blocks (if (vector? content) (vector->list content) content)))
               (string-join
                 (filter (lambda (t) (not (string-null? t)))
                   (map (lambda (blk) (%val 'text blk "")) blocks)
                 ) ;filter
                 "\n"
               ) ;string-join
             ) ;let
            ) ;
            (else "")
      ) ;cond
    ) ;define

    (define (%user-block->gemini-part b)
      (if (%is-image-block? b)
        (%image-block->gemini-part b)
        (let ((t (if (pi-text-block? b) (pi-text-block-text b) (%val 'text b ""))))
          (list (cons "text" t))
        ) ;let
      ) ;if
    ) ;define

    (define (%build-user-turn content)
      (let* ((raw-blks
               (cond ((vector? content) (vector->list content))
                     ((pair? content) content)
                     ((string? content) (list (list (cons "type" "text") (cons "text" content))))
                     (else '())
               ) ;cond
             ) ;raw-blks
             (has-img? (any %is-image-block? raw-blks))
            ) ;
        (if has-img?
          (let ((parts (map %user-block->gemini-part raw-blks)))
            (list (cons "role" "user") (cons "parts" (list->vector parts)))
          ) ;let
          (let ((text-content (%extract-user-text-content content)))
            (list (cons "role" "user")
              (cons "parts" (vector (list (cons "text" text-content))))
            ) ;list
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    (define (%build-assistant-turn content same-model?)
      (let* ((blks (cond ((vector? content) (vector->list content))
                         ((list? content) content)
                         (else '())
                   ) ;cond
             ) ;blks
             (parts
               (filter
                 (lambda (p) (and p (not (null? p))))
                 (map (lambda (b) (%assistant-block->part (if (pi-block? b) (pi-block->alist b) b) same-model?)) blks)
               ) ;filter
             ) ;parts
            ) ;
        (if (null? parts)
          #f
          (list (cons "role" "model") (cons "parts" (list->vector parts)))
        ) ;if
      ) ;let*
    ) ;define

    (define (%build-single-content msg model-id)
      (let ((role (%val 'role msg "")) (content (%val 'content msg '())))
        (cond
         ((equal? role "user") (%build-user-turn content))
         ((equal? role "assistant")
          (let* ((msg-model (%val 'model msg #f))
                 ;; 同 provider+model 才回放签名（对齐上游 isSameProviderAndModel）。
                 ;; record 消息无 model 字段，视为同模型（record 由本进程构造）
                 (same-model?
                   (and (string? model-id)
                     (or (not msg-model) (equal? msg-model model-id))
                   ) ;and
                 ) ;same-model?
                ) ;
            (%build-assistant-turn content same-model?)
          ) ;let*
         ) ;
         (else #f)
        ) ;cond
      ) ;let
    ) ;define

    (define (%first-blk-text blk)
      (if (string? blk) blk (%val 'text blk ""))
    ) ;define

    (define (%tool-result-text content)
      (cond ((string? content) content)
            ((and (vector? content) (> (vector-length content) 0))
             (%first-blk-text (vector-ref content 0))
            ) ;
            ((pair? content) (%first-blk-text (car content)))
            (else "")
      ) ;cond
    ) ;define

    (define (%tool-result->function-response msg)
      (let* ((tool-name (if (pi-tool-result-message? msg)
                          (pi-tool-result-message-tool-name msg)
                          (%val 'toolName msg "")
                        ) ;if
             ) ;tool-name
             (tool-id (if (pi-tool-result-message? msg)
                        (pi-tool-result-message-tool-call-id msg)
                        (%val 'toolCallId msg "")
                      ) ;if
             ) ;tool-id
             (is-err? (if (pi-tool-result-message? msg)
                        (pi-tool-result-message-is-error msg)
                        (%val 'isError msg #f)
                      ) ;if
             ) ;is-err?
             (raw-c (if (pi-tool-result-message? msg)
                      (pi-tool-result-message-content msg)
                      (%val 'content msg '())
                    ) ;if
             ) ;raw-c
             (c-list
               (cond ((vector? raw-c) (vector->list raw-c))
                     ((pair? raw-c) raw-c)
                     ((string? raw-c) (list (pi-text-block raw-c)))
                     (else '())
               ) ;cond
             ) ;c-list
             (text (%tool-result-text raw-c))
             (img-blks (filter %is-image-block? c-list))
             (image-parts (map %image-block->gemini-part img-blks))
             (resp-text
               (cond
                ((not (string-null? (string-trim-both text))) text)
                ((pair? img-blks) "(see attached image)")
                (else "")
               ) ;cond
             ) ;resp-text
             (resp-obj (if is-err? (list (cons "error" resp-text)) (list (cons "output" resp-text)))
             ) ;resp-obj
             (fc (list (cons "name" tool-name) (cons "response" resp-obj) (cons "id" tool-id))
             ) ;fc
             (fc
               (if (pair? image-parts)
                 (append fc (list (cons "parts" (list->vector image-parts))))
                 fc
               ) ;if
             ) ;fc
            ) ;
        (list (cons "functionResponse" fc))
      ) ;let*
    ) ;define

    (define (%take-tool-results rest parts-acc)
      (if
        (and (pair? rest) (equal? (%val 'role (car rest) "") "toolResult"))
        (%take-tool-results (cdr rest)
          (cons (%tool-result->function-response (car rest)) parts-acc)
        ) ;%take-tool-results
        (values rest parts-acc)
      ) ;if
    ) ;define

    (define* (gemini-build-contents messages (model-id #f))
      (let loop
        ((rest messages) (acc '()))
        (if (null? rest)
          (list->vector (reverse acc))
          (let* ((msg (car rest)) (role (%val 'role msg "")))
            (if (equal? role "toolResult")
              (let*-values (((rem-rest rparts) (%take-tool-results rest '()))
                            ((parts-vec) (list->vector (reverse rparts)))
                            ((merged-turn) (list (cons "role" "user") (cons "parts" parts-vec)))
                           ) ;
                (loop rem-rest (cons merged-turn acc))
              ) ;let*-values
              (let ((c (%build-single-content msg model-id)))
                (loop (cdr rest) (if c (cons c acc) acc))
              ) ;let
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define*

    (define (gemini-build-request-body contents sys-inst tool-decls)
      (let* ((req (list (cons "contents" contents)))
             (req
               (if
                 (and (string? sys-inst) (not (string-null? (string-trim-both sys-inst))))
                 (cons
                   (cons "systemInstruction"
                     (list (cons "role" "user")
                       (cons "parts" (vector (list (cons "text" sys-inst))))
                     ) ;list
                   ) ;cons
                   req
                 ) ;cons
                 req
               ) ;if
             ) ;req
             (has-tools?
               (or (and (pair? tool-decls) (> (length tool-decls) 0))
                 (and (vector? tool-decls) (> (vector-length tool-decls) 0))
               ) ;or
             ) ;has-tools?
             (req
               (if has-tools?
                 (let* ((tools-val
                          (if (vector? tool-decls)
                            tool-decls
                            (vector (list (cons "functionDeclarations" (list->vector tool-decls))))
                          ) ;if
                        ) ;tools-val
                        (tc-val
                          (list (cons "functionCallingConfig" (list (cons "mode" "AUTO"))))
                        ) ;tc-val
                       ) ;
                   (cons (cons "toolConfig" tc-val) (cons (cons "tools" tools-val) req))
                 ) ;let*
                 req
               ) ;if
             ) ;req
            ) ;
        (reverse req)
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
