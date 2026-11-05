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

    ;; 消息 role 统一以字符串比较；非 alist 原样跳过（防御）
    (define (%msg-role m)
      (if (pair? m) (%val "role" m "") "")
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

    ;; 归一化单个 toolCall block 并返回 (values new-block new-id-entry-or-#f)
    (define (%normalize-one-block blk)
      (if (and (pair? blk) (equal? (%val "type" blk "") "toolCall"))
        (let* ((old-id (%val "id" blk "")) (new-id (%normalize-tool-call-id old-id)))
          (values (%replace-kv blk "id" new-id) (cons old-id new-id))
        ) ;let*
        (values blk #f)
      ) ;if
    ) ;define

    ;; 归一化 assistant 消息中的 toolCall 块，返回 (values new-blocks new-id-map)
    (define (%normalize-assistant-blocks blocks id-map)
      (let loop
        ((rest blocks) (acc '()) (m id-map))
        (if (null? rest)
          (values (reverse acc) m)
          (let-values (((new-blk id-entry) (%normalize-one-block (car rest))))
            (loop (cdr rest) (cons new-blk acc) (if id-entry (cons id-entry m) m))
          ) ;let-values
        ) ;if
      ) ;let
    ) ;define

    ;; 处理 assistant 消息：若 error/aborted 则丢弃；若需要则归一化 toolCall id
    (define (%transform-assistant msg normalize? id-map)
      (let ((stop (%val "stopReason" msg "")))
        (cond ((or (equal? stop "error") (equal? stop "aborted")) (values #f id-map))
              ((not normalize?) (values msg id-map))
              (else
                (let ((content (%val "content" msg '())))
                  (let-values (((new-blocks new-map) (%normalize-assistant-blocks content id-map)))
                    (values (%replace-kv msg "content" new-blocks) new-map)
                  ) ;let-values
                ) ;let
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

    ;; ---- 第一遍：error/aborted 过滤 + toolCall id 归一化 ----
    (define (%transform-one msg normalize? id-map)
      (let ((role (%msg-role msg)))
        (cond ((equal? role "assistant") (%transform-assistant msg normalize? id-map))
              ((equal? role "toolResult") (%transform-tool-result msg normalize? id-map))
              (else (values msg id-map))
        ) ;cond
      ) ;let
    ) ;define

    (define (%transform-pass messages normalize?)
      (let loop
        ((rest messages) (acc '()) (id-map '()))
        (if (null? rest)
          (values (reverse acc) id-map)
          (let-values (((m new-map) (%transform-one (car rest) normalize? id-map)))
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

    (define (%assistant-tool-calls msg)
      (if (equal? (%msg-role msg) "assistant")
        (filter
          (lambda (b) (and (pair? b) (equal? (%val "type" b "") "toolCall")))
          (%val "content" msg '())
        ) ;filter
        '()
      ) ;if
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
          (let-values (((transformed id-map) (%transform-pass messages normalize?)))
            (%orphan-pass transformed)
          ) ;let-values
        ) ;let
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
