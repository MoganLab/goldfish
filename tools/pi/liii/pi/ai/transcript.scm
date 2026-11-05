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

    ;; ---- Gemini 模型版本判定（对齐上游 requiresToolCallId 的 gemini 部分） ----
    ;; gemini-3-* / gemini-3.1-* / gemini-live-3-* 等主版本 >= 3 的模型
    ;; 要求显式 toolCall id 且 id 须匹配 [a-zA-Z0-9_-]（<= 64 字符）
    (define (%gemini-major-version model-id)
      (let* ((s (if (string? model-id) (string-downcase model-id) ""))
             (prefices '("gemini-" "gemini-live-"))
            ) ;
        (let loop
          ((ps prefices))
          (cond ((null? ps) #f)
                ((and (>= (string-length s) (string-length (car ps)))
                   (string=? (substring s 0 (string-length (car ps))) (car ps))
                 ) ;and
                 (let* ((rest (substring s (string-length (car ps)) (string-length s)))
                        (digits
                          (let dl
                            ((i 0))
                            (if (and (< i (string-length rest)) (char-numeric? (string-ref rest i)))
                              (dl (+ i 1))
                              i
                            ) ;if
                          ) ;let
                        ) ;digits
                       ) ;
                   (if (> digits 0) (string->number (substring rest 0 digits)) #f)
                 ) ;let*
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

    ;; id 字符归一化：非法字符 → _，超 64 截断（对齐上游）
    (define (%normalize-tool-call-id id)
      (if (not (string? id))
        "tool_call_0"
        (let ((res
                (list->string
                  (map
                    (lambda (c)
                      (if (or (char-alphabetic? c) (char-numeric? c) (char=? c #\_) (char=? c #\-))
                        c
                        #\_
                      ) ;if
                    ) ;lambda
                    (string->list id)
                  ) ;map
                ) ;list->string
              ) ;res
             ) ;
          (if (> (string-length res) 64) (substring res 0 64) res)
        ) ;let
      ) ;if
    ) ;define

    ;; ---- 第一遍：error/aborted 过滤 + toolCall id 归一化 ----
    ;; 返回两个值：转换后消息列表、id 映射 alist（旧 id . 新 id）
    ;; id 映射后者覆盖前者（与上游 Map 语义一致）
    (define (%transform-one msg normalize? id-map)
      ;; 返回 (values new-msg-or-#f new-id-map)
      (let ((role (%msg-role msg)))
        (cond
         ((equal? role "assistant")
          (let ((stop (%val "stopReason" msg "")))
            (if (or (equal? stop "error") (equal? stop "aborted"))
              (values #f id-map)
              (let ((content (%val "content" msg '())))
                (if (not normalize?)
                  (values msg id-map)
                  (let loop
                    ((blks content) (acc '()) (m id-map))
                    (if (null? blks)
                      (let ((new-msg
                              (map
                                (lambda (e) (if (equal? (car e) "content") (cons "content" (reverse acc)) e))
                                msg
                              ) ;map
                            ) ;new-msg
                           ) ;
                        (values new-msg m)
                      ) ;let
                      (let ((blk (car blks)))
                        (if (and (pair? blk) (equal? (%val "type" blk "") "toolCall"))
                          (let* ((old-id (%val "id" blk "")) (new-id (%normalize-tool-call-id old-id)))
                            (loop (cdr blks)
                              (cons
                                (map
                                  (lambda (e) (if (equal? (car e) "id") (cons "id" new-id) e))
                                  blk
                                ) ;map
                                acc
                              ) ;cons
                              (cons (cons old-id new-id) m)
                            ) ;loop
                          ) ;let*
                          (loop (cdr blks) (cons blk acc) m)
                        ) ;if
                      ) ;let
                    ) ;if
                  ) ;let
                ) ;if
              ) ;let
            ) ;if
          ) ;let
         ) ;
         ((equal? role "toolResult")
          (if (not normalize?)
            (values msg id-map)
            (let* ((old-id (%val "toolCallId" msg "")) (hit (assoc old-id id-map)))
              (if hit
                (values
                  (map
                    (lambda (e) (if (equal? (car e) "toolCallId") (cons "toolCallId" (cdr hit)) e))
                    msg
                  ) ;map
                  id-map
                ) ;values
                (values msg id-map)
              ) ;if
            ) ;let*
          ) ;if
         ) ;
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

    ;; pending: 待回应 toolCall 块列表（按出现顺序）；answered: 已出现的 toolCallId
    (define (%orphan-pass messages)
      (let loop
        ((rest messages) (acc '()) (pending '()) (answered '()))
        ;; 冲刷 pending：为每个未回应调用追加合成结果
        (define (flush acc pending answered)
          (let f
            ((ps pending) (a acc) (ans answered))
            (if (null? ps)
              (values a ans)
              (let ((tc (car ps)))
                (if (member (%val "id" tc "") ans)
                  (f (cdr ps) a ans)
                  (f (cdr ps)
                    (cons (%make-synthetic-tool-result (%val "id" tc "") (%val "name" tc "")) a)
                    ans
                  ) ;f
                ) ;if
              ) ;let
            ) ;if
          ) ;let
        ) ;define
        (if (null? rest)
          (let-values (((a ans) (flush acc pending answered)))
            (reverse a)
          ) ;let-values
          (let* ((msg (car rest)) (role (%msg-role msg)))
            (cond
             ((equal? role "assistant")
              (let-values (((a ans) (flush acc pending answered)))
                (loop (cdr rest) (cons msg a) (%assistant-tool-calls msg) '())
              ) ;let-values
             ) ;
             ((equal? role "toolResult")
              (loop (cdr rest)
                (cons msg acc)
                pending
                (cons (%val "toolCallId" msg "") answered)
              ) ;loop
             ) ;
             ((equal? role "user")
              (let-values (((a ans) (flush acc pending answered)))
                (loop (cdr rest) (cons msg a) '() '())
              ) ;let-values
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
