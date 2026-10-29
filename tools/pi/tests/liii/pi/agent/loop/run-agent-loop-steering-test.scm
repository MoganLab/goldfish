(import (scheme base)
  (liii go)
  (liii list)
  (liii check)
  (liii pi agent loop)
  (liii pi ai message)
) ;import

(check-set-mode! 'report-failed)

;; 构造 stop-reason 为 toolUse 的 assistant 响应
(define (make-tool-use-response)
  (pi-assistant-message
    (list (list (cons 'type "text") (cons 'text "calling tool"))
      (list (cons 'type "toolCall")
        (cons 'id "call_1")
        (cons 'name "bash")
        (cons 'arguments (list (cons "command" "echo hi")))
      ) ;list
    ) ;list
    :stop-reason "toolUse"
    :usage '((input . 1) (output . 1) (total . 2))
  ) ;pi-assistant-message
) ;define

(define (make-text-response text)
  (pi-assistant-message
    (list (list (cons 'type "text") (cons 'text text)))
    :stop-reason "stop"
    :usage '((input . 1) (output . 1) (total . 2))
  ) ;pi-assistant-message
) ;define

(define (make-emit-collector)
  (let ((events '()))
    (lambda (ev-type . args)
      (if (eq? ev-type 'dump)
        (reverse events)
        (begin
          (set! events (cons (cons ev-type args) events))
          #t
        ) ;begin
      ) ;if
    ) ;lambda
  ) ;let
) ;define

;; 消息角色序列（用于断言注入顺序）
(define (roles-of msgs)
  (map pi-message-role msgs)
) ;define

;; 提取 user 消息文本序列
(define (user-texts-of msgs)
  (map pi-user-message-content
    (filter pi-user-message? msgs)
  ) ;map
) ;define

;; 执行工具恒成功
(define (ok-exec name . args)
  (list (cons 'content
         (list (list (cons 'type "text") (cons 'text "tool output")))
       ) ;list
    (cons 'isError #f)
  ) ;list
) ;define

;; -----------------------------------------------------------------------------
;; 测试项 1: 工具批次结束后注入 steering，消息位于 tool result 之后、
;; 下一轮请求之前；LLM 在下一轮看到完整上下文
;; -----------------------------------------------------------------------------
(let* ((req-count (vector 0))
       (last-req-msgs (vector #f))
       (request-fn
         (lambda (provider config msgs sys-prompt tools on-ev timeout)
           (vector-set! req-count 0 (+ 1 (vector-ref req-count 0)))
           (vector-set! last-req-msgs 0 msgs)
           (if (= (vector-ref req-count 0) 1)
             (make-tool-use-response)
             (make-text-response "done")
           ) ;if
         ) ;lambda
        )
       ;; 轮询#1（run 启动）无消息；轮询#2（工具批次后、下轮请求前）返回一条，之后恒空
       (poll-count (vector 0))
       (get-steering
         (lambda ()
           (vector-set! poll-count 0 (+ 1 (vector-ref poll-count 0)))
           (if (= (vector-ref poll-count 0) 2) '("换个思路") '())
         ) ;lambda
        )
       (emit (make-emit-collector))
       (cfg (list (cons 'provider "test")
              (cons 'tools '("bash"))
              (cons 'settings '())
              (cons 'execute-tool-call ok-exec)
              (cons 'get-steering-messages get-steering)
            ) ;list
           )
        ) ;
  (let ((res-msgs (run-agent-loop cfg (list (pi-user-message "hi")) "sys" emit 10 5
                    600 #f request-fn
                  ) ;run-agent-loop
               ) ;res-msgs
           ) ;let
    ;; 两轮请求：第 1 轮工具调用，第 2 轮文本回复
    (check (vector-ref req-count 0) => 2)
    ;; 第 2 轮请求上下文：user → assistant(toolUse) → toolResult → user(steering)
    (check (roles-of (vector-ref last-req-msgs 0))
      => '("user" "assistant" "toolResult" "user")
    ) ;check
    (check (user-texts-of (vector-ref last-req-msgs 0)) => '("hi" "换个思路"))
    ;; 循环返回的最终消息列表：hi + toolUse + toolResult + steering + 回复 共 5 条
    (check (length res-msgs) => 5)
    (check (user-texts-of res-msgs) => '("hi" "换个思路"))
    ;; 注入消息经过 emit（message_start / message_end）上屏
    (check-true
      (pair?
        (filter
          (lambda (ev)
            (and (eq? (car ev) 'message_end)
              (pair? (cdr ev))
              (pi-user-message? (cadr ev))
              (equal? (pi-user-message-content (cadr ev)) "换个思路")
            ) ;and
          ) ;lambda
          (emit 'dump)
        ) ;filter
      ) ;pair?
    ) ;check-true
  ) ;let
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 2: 首轮请求前的轮询：run 启动时已排队的 steering 一并进入
;; 第一次请求上下文（对齐上游 loop 启动时的 steering 检查）
;; -----------------------------------------------------------------------------
(let* ((req-count (vector 0))
       (first-req-msgs (vector #f))
       (request-fn
         (lambda (provider config msgs sys-prompt tools on-ev timeout)
           (vector-set! req-count 0 (+ 1 (vector-ref req-count 0)))
           (when (= (vector-ref req-count 0) 1)
             (vector-set! first-req-msgs 0 msgs)
           ) ;when
           (make-text-response "done")
         ) ;lambda
        )
       ;; 启动即有 1 条 steering，第 2 次轮询返回空
       (poll-count (vector 0))
       (get-steering
         (lambda ()
           (vector-set! poll-count 0 (+ 1 (vector-ref poll-count 0)))
           (if (= (vector-ref poll-count 0) 1) '("补充要求") '())
         ) ;lambda
        )
       (emit (make-emit-collector))
       (cfg (list (cons 'provider "test")
              (cons 'tools '())
              (cons 'settings '())
              (cons 'get-steering-messages get-steering)
            ) ;list
           )
        ) ;
  (run-agent-loop cfg (list (pi-user-message "hi")) "sys" emit 10 5
    600 #f request-fn
  ) ;run-agent-loop
  ;; 第 1 次请求就带上初始消息 + steering 共 2 条
  (check (roles-of (vector-ref first-req-msgs 0)) => '("user" "user"))
  (check (user-texts-of (vector-ref first-req-msgs 0)) => '("hi" "补充要求"))
  ;; 无工具调用：仍只有 1 次请求即自然结束
  (check (vector-ref req-count 0) => 1)
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 3: 纯文本轮结束后 steering 到达：继续下一轮而不是结束
;; -----------------------------------------------------------------------------
(let* ((req-count (vector 0))
       (request-fn
         (lambda (provider config msgs sys-prompt tools on-ev timeout)
           (vector-set! req-count 0 (+ 1 (vector-ref req-count 0)))
           (make-text-response (string-append "reply-" (number->string (vector-ref req-count 0))))
         ) ;lambda
        )
       ;; 轮询#1（run 启动）无消息；轮询#2（文本轮后、自然停止前）返回一条，之后恒空
       (poll-count (vector 0))
       (get-steering
         (lambda ()
           (vector-set! poll-count 0 (+ 1 (vector-ref poll-count 0)))
           (if (= (vector-ref poll-count 0) 2) '("继续总结") '())
         ) ;lambda
        )
       (emit (make-emit-collector))
       (cfg (list (cons 'provider "test")
              (cons 'tools '())
              (cons 'settings '())
              (cons 'get-steering-messages get-steering)
            ) ;list
           )
        ) ;
  (let ((res-msgs (run-agent-loop cfg (list (pi-user-message "hi")) "sys" emit 10 5
                    600 #f request-fn
                  ) ;run-agent-loop
               ) ;res-msgs
           ) ;let
    (check (vector-ref req-count 0) => 2)
    (check (length res-msgs) => 4)
  ) ;let
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 4: steering 回调抛异常时循环不受影响（契约：不得抛出，防御性兜底）
;; -----------------------------------------------------------------------------
(let* ((req-count (vector 0))
       (request-fn
         (lambda (provider config msgs sys-prompt tools on-ev timeout)
           (vector-set! req-count 0 (+ 1 (vector-ref req-count 0)))
           (make-text-response "done")
         ) ;lambda
        )
       (emit (make-emit-collector))
       (cfg (list (cons 'provider "test")
              (cons 'tools '())
              (cons 'settings '())
              (cons 'get-steering-messages (lambda () (error 'boom "poll failed")))
            ) ;list
           )
        ) ;
  (let ((res-msgs (run-agent-loop cfg (list (pi-user-message "hi")) "sys" emit 10 5
                    600 #f request-fn
                  ) ;run-agent-loop
               ) ;res-msgs
           ) ;let
    (check (vector-ref req-count 0) => 1)
    (check (length res-msgs) => 2)
  ) ;let
) ;let*

(check-report)
