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
    (liii pi ai api google-generative-ai thought-signature)
  ) ;import
  (export gemini-build-contents gemini-build-request-body)
  (begin

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

    (define (%to-str x)
      (cond ((string? x) x)
            ((symbol? x) (symbol->string x))
            (else #f)
      ) ;cond
    ) ;define

    (define (%msg-model msg)
      (cond ((pi-assistant-message? msg) (pi-assistant-message-model msg))
            (else (or (%val 'model msg #f) (%val 'model-id msg #f)))
      ) ;cond
    ) ;define

    (define (%msg-provider msg)
      (cond ((pi-assistant-message? msg) (pi-assistant-message-provider msg))
            (else (%val 'provider msg #f))
      ) ;cond
    ) ;define

    ;; 判定历史消息是否与当前请求目标属于同一模型（对齐上游 isSameProviderAndModel）。
    ;; 要求双方的 provider 和 model 必须明确存在且完全一致；
    ;; 无明确模型信息时默认按跨模型安全策略处理（不回放签名，丢弃思维链）。
    (define (%is-same-model? msg model-id provider)
      (let ((msg-m (%to-str (%msg-model msg)))
            (tgt-m (%to-str model-id))
            (msg-p (%to-str (%msg-provider msg)))
            (tgt-p (%to-str provider))
           ) ;
        (and (string? tgt-m)
          (string? msg-m)
          (equal? msg-m tgt-m)
          (string? tgt-p)
          (string? msg-p)
          (equal? msg-p tgt-p)
        ) ;and
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

    (define (%build-assistant-turn content same-model? model-id)
      (let* ((blks (cond ((vector? content) (vector->list content))
                         ((list? content) content)
                         (else '())
                   ) ;cond
             ) ;blks
             (parts
               (filter
                 (lambda (p) (and p (not (null? p))))
                 (map (lambda (b) (gemini-assistant-block->part b same-model? model-id)) blks)
               ) ;filter
             ) ;parts
            ) ;
        (if (null? parts)
          #f
          (list (cons "role" "model") (cons "parts" (list->vector parts)))
        ) ;if
      ) ;let*
    ) ;define

    (define (%build-single-content msg model-id provider)
      (let ((role (if (pi-message? msg) (pi-message-role msg) (%val 'role msg "")))
            (content
              (if (pi-message? msg)
                (if (pi-assistant-message? msg)
                  (pi-assistant-message-content msg)
                  (if (pi-user-message? msg)
                    (pi-user-message-content msg)
                    (%val 'content msg '())
                  ) ;if
                ) ;if
                (%val 'content msg '())
              ) ;if
            ) ;content
           ) ;
        (cond ((equal? role "user") (%build-user-turn content))
              ((equal? role "assistant")
               (let ((same-model? (%is-same-model? msg model-id provider)))
                 (%build-assistant-turn content same-model? model-id)
               ) ;let
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
        (and (pair? rest)
          (equal? (if (pi-message? (car rest))
                    (pi-message-role (car rest))
                    (%val 'role (car rest) "")
                  ) ;if
            "toolResult"
          ) ;equal?
        ) ;and
        (%take-tool-results (cdr rest)
          (cons (%tool-result->function-response (car rest)) parts-acc)
        ) ;%take-tool-results
        (values rest parts-acc)
      ) ;if
    ) ;define

    (define* (gemini-build-contents messages (model-id #f) (provider "google"))
      (let loop
        ((rest messages) (acc '()))
        (if (null? rest)
          (list->vector (reverse acc))
          (let* ((msg (car rest))
                 (role (if (pi-message? msg) (pi-message-role msg) (%val 'role msg "")))
                ) ;
            (if (equal? role "toolResult")
              (let*-values (((rem-rest rparts) (%take-tool-results rest '()))
                            ((parts-vec) (list->vector (reverse rparts)))
                            ((merged-turn) (list (cons "role" "user") (cons "parts" parts-vec)))
                           ) ;
                (loop rem-rest (cons merged-turn acc))
              ) ;let*-values
              (let ((c (%build-single-content msg model-id provider)))
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
