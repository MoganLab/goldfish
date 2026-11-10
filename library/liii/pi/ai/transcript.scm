;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 请求前转录规范化（对应上游 pi-ai 的 api/transform-messages.ts）。
;; 在构建 provider 请求体前统一规范化消息列表，仅作用于请求构建，
;; 不改会话持久化。保证请求前缀跨轮稳定、孤儿 toolCall 不再击穿缓存。
(define-library (liii pi ai transcript)
  (import (scheme base)
    (scheme char)
    (liii string)
    (liii list)
    (liii pi ai message)
    (liii pi ai transcript cross-model)
  ) ;import
  (export pi-normalize-transcript pi-transcript-normalize-tool-call-id?)
  (begin

    (define (%val key alist default-val)
      (if (not (pair? alist))
        default-val
        (let* ((str-k (if (symbol? key) (symbol->string key) key))
               (sym-k (if (string? key) (string->symbol key) key))
               (p (or (assoc key alist) (assoc str-k alist) (assoc sym-k alist)))
              ) ;
          (if p (cdr p) default-val)
        ) ;let*
      ) ;if
    ) ;define

    ;; 消息 role 统一以字符串比较；支持 record 与 alist；非 alist/record 原样跳过（防御）
    (define (%msg-role m)
      (cond ((pi-message? m) (pi-message-role m))
            ((pair? m) (%val "role" m ""))
            (else "")
      ) ;cond
    ) ;define

    ;; 提取字符串起始连续数字构成的数值，未匹配返回 #f
    (define (%extract-leading-digits s)
      (let dl
        ((i 0))
        (cond
         ((and (< i (string-length s)) (char-numeric? (string-ref s i))) (dl (+ i 1)))
         ((> i 0) (string->number (substring s 0 i)))
         (else #f)
        ) ;cond
      ) ;let
    ) ;define

    ;; ---- Gemini 模型版本判定（对齐上游 requiresToolCallId 的 gemini 部分） ----
    ;; gemini-3-* / gemini-3.1-* / gemini-live-3-* 等主版本 >= 3 的模型
    ;; 要求显式 toolCall id 且 id 须匹配 [a-zA-Z0-9_-]（<= 64 字符）
    (define (%gemini-major-version model-id)
      (let* ((s (if (string? model-id) (string-downcase model-id) ""))
             (prefices '("gemini-live-" "gemini-"))
            ) ;
        (let loop
          ((ps prefices))
          (cond ((null? ps) #f)
                ((string-starts? s (car ps))
                 (let ((rest (substring s (string-length (car ps)) (string-length s))))
                   (%extract-leading-digits rest)
                 ) ;let
                ) ;
                (else (loop (cdr ps)))
          ) ;cond
        ) ;let
      ) ;let*
    ) ;define

    ;; 当前请求是否需要归一化 toolCall id：仅 Gemini 3+
    (define (pi-transcript-normalize-tool-call-id? config)
      (let ((api (%val "provider-api" config (%val 'provider-api config "")))
            (model-id (%val "model-id" config (%val 'model-id config "")))
           ) ;
        (and (equal? api "google-generative-ai")
          (let ((v (%gemini-major-version model-id)))
            (and (number? v) (>= v 3))
          ) ;let
        ) ;and
      ) ;let
    ) ;define

    (define (%char-legal-for-id? c)
      (or (char-alphabetic? c) (char-numeric? c) (char=? c #\_) (char=? c #\-))
    ) ;define

    ;; id 字符归一化：非法字符 → _，超 64 截断（对齐上游）
    (define (%normalize-tool-call-id id)
      (if (not (string? id))
        "tool_call_0"
        (let* ((chars
                 (map (lambda (c) (if (%char-legal-for-id? c) c #\_)) (string->list id))
               ) ;chars
               (res (list->string chars))
              ) ;
          (if (> (string-length res) 64) (substring res 0 64) res)
        ) ;let*
      ) ;if
    ) ;define

    (define (%replace-kv alist key new-val)
      (map
        (lambda (e) (if (equal? (car e) key) (cons key new-val) e))
        alist
      ) ;map
    ) ;define

    ;; 转换单个内容块：跨模型时过滤 thinking，剥离 toolCall 的 thoughtSignature 与 text 的签名
    (define (%transform-one-block blk normalize? same-model? id-map)
      (let ((nb (pi-transcript-normalize-block blk same-model?)))
        (if (not nb)
          (values #f id-map)
          (if (and normalize? (equal? (%val "type" nb "") "toolCall"))
            (let* ((old-id (%val "id" nb "")) (new-id (%normalize-tool-call-id old-id)))
              (values (%replace-kv nb "id" new-id) (cons (cons old-id new-id) id-map))
            ) ;let*
            (values nb id-map)
          ) ;if
        ) ;if
      ) ;let
    ) ;define

    ;; 转换 assistant 消息中的内容块，返回 (values new-blocks new-id-map)
    (define (%transform-assistant-blocks blocks normalize? same-model? id-map)
      (let loop
        ((rest blocks) (acc '()) (m id-map))
        (if (null? rest)
          (values (reverse acc) m)
          (let-values (((new-blk new-m) (%transform-one-block (car rest) normalize? same-model? m)))
            (loop (cdr rest) (if new-blk (cons new-blk acc) acc) new-m)
          ) ;let-values
        ) ;if
      ) ;let
    ) ;define

    ;; 处理 assistant 消息：若 error/aborted 则丢弃；跨模型过滤与按需归一化 toolCall id
    (define (%transform-assistant msg normalize? same-model? id-map)
      (let ((stop (%val "stopReason" msg "")))
        (cond ((or (equal? stop "error") (equal? stop "aborted")) (values #f id-map))
              (else
                (let* ((raw-content (%val "content" msg '()))
                       (is-vec? (vector? raw-content))
                       (content (cond ((vector? raw-content) (vector->list raw-content))
                                      ((pair? raw-content) raw-content)
                                      (else '())
                                ) ;cond
                       ) ;content
                      ) ;
                  (let-values (((new-blocks new-map)
                                (%transform-assistant-blocks content normalize? same-model? id-map)
                               ) ;
                              ) ;
                    (let ((final-blocks (if is-vec? (list->vector new-blocks) new-blocks)))
                      (values (%replace-kv msg "content" final-blocks) new-map)
                    ) ;let
                  ) ;let-values
                ) ;let*
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    ;; 处理 toolResult 消息：按 id-map 替换 toolCallId
    (define (%transform-tool-result msg normalize? id-map)
      (if (not normalize?)
        (values msg id-map)
        (let* ((old-id (%val "toolCallId" msg "")) (hit (assoc old-id id-map)))
          (if hit
            (values (%replace-kv msg "toolCallId" (cdr hit)) id-map)
            (values msg id-map)
          ) ;if
        ) ;let*
      ) ;if
    ) ;define

    ;; ---- 第一遍：error/aborted 过滤 + 跨模型转换 + toolCall id 归一化 ----
    (define (%transform-one raw-msg normalize? config id-map)
      (let* ((msg (if (pi-message? raw-msg) (pi-message->alist raw-msg) raw-msg))
             (role (%msg-role msg))
            ) ;
        (cond
         ((equal? role "assistant")
          (let ((same-model? (pi-transcript-same-model? msg config)))
            (%transform-assistant msg normalize? same-model? id-map)
          ) ;let
         ) ;
         ((equal? role "toolResult") (%transform-tool-result msg normalize? id-map))
         (else (values msg id-map))
        ) ;cond
      ) ;let*
    ) ;define

    (define (%transform-pass messages normalize? config)
      (let loop
        ((rest messages) (acc '()) (id-map '()))
        (if (null? rest)
          (values (reverse acc) id-map)
          (let-values (((m new-map) (%transform-one (car rest) normalize? config id-map)))
            (loop (cdr rest) (if m (cons m acc) acc) new-map)
          ) ;let-values
        ) ;if
      ) ;let
    ) ;define

    ;; ---- 第二遍：孤儿 toolCall 合成 toolResult ----
    (define (%make-synthetic-tool-result id name)
      (list (cons "role" "toolResult")
        (cons "toolCallId" id)
        (cons "toolName" name)
        (cons "content"
          (list (list (cons "type" "text") (cons "text" "No result provided")))
        ) ;cons
        (cons "isError" #t)
      ) ;list
    ) ;define

    (define (%assistant-tool-calls raw-msg)
      (let ((msg (if (pi-message? raw-msg) (pi-message->alist raw-msg) raw-msg)))
        (if (equal? (%msg-role msg) "assistant")
          (let ((content (%val "content" msg '())))
            (filter
              (lambda (b) (and (pair? b) (equal? (%val "type" b "") "toolCall")))
              (if (vector? content) (vector->list content) content)
            ) ;filter
          ) ;let
          '()
        ) ;if
      ) ;let
    ) ;define

    ;; 冲刷 pending 中尚未被 answered 的 toolCall
    (define (%flush-pending pending answered acc)
      (let loop
        ((ps pending) (a acc))
        (if (null? ps)
          a
          (let* ((tc (car ps)) (id (%val "id" tc "")))
            (if (member id answered)
              (loop (cdr ps) a)
              (let ((synth (%make-synthetic-tool-result id (%val "name" tc ""))))
                (loop (cdr ps) (cons synth a))
              ) ;let
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    ;; pending: 待回应 toolCall 块列表（按出现顺序）；answered: 已出现的 toolCallId
    (define (%orphan-pass messages)
      (let loop
        ((rest messages) (acc '()) (pending '()) (answered '()))
        (if (null? rest)
          (reverse (%flush-pending pending answered acc))
          (let* ((msg (car rest)) (role (%msg-role msg)))
            (cond
             ((equal? role "assistant")
              (let ((flushed (%flush-pending pending answered acc)))
                (loop (cdr rest) (cons msg flushed) (%assistant-tool-calls msg) '())
              ) ;let
             ) ;
             ((equal? role "toolResult")
              (loop (cdr rest)
                (cons msg acc)
                pending
                (cons (%val "toolCallId" msg "") answered)
              ) ;loop
             ) ;
             ((equal? role "user")
              (let ((flushed (%flush-pending pending answered acc)))
                (loop (cdr rest) (cons msg flushed) '() '())
              ) ;let
             ) ;
             (else (loop (cdr rest) (cons msg acc) pending answered))
            ) ;cond
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (pi-normalize-transcript messages config)
      (if (null? messages)
        '()
        (let ((normalize? (pi-transcript-normalize-tool-call-id? config)))
          (let-values (((transformed id-map) (%transform-pass messages normalize? config)))
            (%orphan-pass transformed)
          ) ;let-values
        ) ;let
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
