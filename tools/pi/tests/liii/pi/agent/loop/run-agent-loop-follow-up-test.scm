(import (scheme base)
  (liii go)
  (liii list)
  (liii check)
  (liii pi agent loop)
  (liii pi ai message)
) ;import

(check-set-mode! 'report-failed)

(define (make-text-response text)
  (pi-assistant-message
    (list (list (cons 'type "text") (cons 'text text)))
    :stop-reason "stop"
    :usage '((input . 1) (output . 1) (total . 2))
  ) ;pi-assistant-message
) ;define

(define (make-error-response)
  (pi-assistant-message '()
    :stop-reason "error"
    :error-message "provider exploded"
    :usage '((input . 1) (output . 1) (total . 2))
  ) ;pi-assistant-message
) ;define

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

(define (roles-of msgs)
  (map pi-message-role msgs)
) ;define

(define (user-texts-of msgs)
  (map pi-user-message-content
    (filter pi-user-message? msgs)
  ) ;map
) ;define

;; -----------------------------------------------------------------------------
;; 测试项 1: 自然停止时消费 follow-up：注入 user 消息继续下一轮，
;; 下一轮自然停止且无新 follow-up 时才真正结束
;; -----------------------------------------------------------------------------
(let* ((req-count (vector 0))
       (last-req-msgs (vector #f))
       (request-fn
         (lambda (provider config msgs sys-prompt tools on-ev timeout)
           (vector-set! req-count 0 (+ 1 (vector-ref req-count 0)))
           (vector-set! last-req-msgs 0 msgs)
           (make-text-response (string-append "reply-" (number->string (vector-ref req-count 0))))
         ) ;lambda
        )
       ;; follow-up 第 1 次轮询返回一条，之后恒空
       (fu-count (vector 0))
       (get-follow-up
         (lambda ()
           (vector-set! fu-count 0 (+ 1 (vector-ref fu-count 0)))
           (if (= (vector-ref fu-count 0) 1) '("再总结一下") '())
         ) ;lambda
        )
       (emit (make-emit-collector))
       (cfg (list (cons 'provider "test")
              (cons 'tools '())
              (cons 'settings '())
              (cons 'get-follow-up-messages get-follow-up)
            ) ;list
           )
        ) ;
  (let ((res-msgs (run-agent-loop cfg (list (pi-user-message "hi")) "sys" emit 10 5
                    600 #f request-fn
                  ) ;run-agent-loop
               ) ;res-msgs
           ) ;let
    (check (vector-ref req-count 0) => 2)
    ;; 第 2 轮请求上下文：user → assistant → user(follow-up)
    (check (roles-of (vector-ref last-req-msgs 0)) => '("user" "assistant" "user"))
    (check (user-texts-of (vector-ref last-req-msgs 0)) => '("hi" "再总结一下"))
    ;; 最终消息列表：hi + 回复1 + follow-up + 回复2 共 4 条
    (check (length res-msgs) => 4)
    (check (user-texts-of res-msgs) => '("hi" "再总结一下"))
    ;; follow-up 消息经过 emit 上屏
    (check-true
      (pair?
        (filter
          (lambda (ev)
            (and (eq? (car ev) 'message_end)
              (pair? (cdr ev))
              (pi-user-message? (cadr ev))
              (equal? (pi-user-message-content (cadr ev)) "再总结一下")
            ) ;and
          ) ;lambda
          (emit 'dump)
        ) ;filter
      ) ;pair?
    ) ;check-true
  ) ;let
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 2: steering 优先于 follow-up：steering 非空时本轮不轮询 follow-up
;; -----------------------------------------------------------------------------
(let* ((req-count (vector 0))
       (request-fn
         (lambda (provider config msgs sys-prompt tools on-ev timeout)
           (vector-set! req-count 0 (+ 1 (vector-ref req-count 0)))
           (make-text-response "done")
         ) ;lambda
        )
       (steer-count (vector 0))
       (get-steering
         (lambda ()
           (vector-set! steer-count 0 (+ 1 (vector-ref steer-count 0)))
           ;; 轮询#2（文本轮停止前）返回一条，其余恒空
           (if (= (vector-ref steer-count 0) 2) '("改这里") '())
         ) ;lambda
        )
       (fu-count (vector 0))
       (get-follow-up
         (lambda ()
           (vector-set! fu-count 0 (+ 1 (vector-ref fu-count 0)))
           ;; 第 1 次轮询返回一条，之后恒空
           (if (= (vector-ref fu-count 0) 1) '("follow-up 后到") '())
         ) ;lambda
        )
       (emit (make-emit-collector))
       (cfg (list (cons 'provider "test")
              (cons 'tools '())
              (cons 'settings '())
              (cons 'get-steering-messages get-steering)
              (cons 'get-follow-up-messages get-follow-up)
            ) ;list
           )
        ) ;
  (let ((res-msgs (run-agent-loop cfg (list (pi-user-message "hi")) "sys" emit 10 5
                    600 #f request-fn
                  ) ;run-agent-loop
               ) ;res-msgs
           ) ;let
    ;; 第 2 轮由 steering 驱动；第 3 轮由 follow-up 驱动：
    ;; 第 1 次停止检查时 steering 非空，follow-up 未被轮询（优先级）
    (check (vector-ref req-count 0) => 3)
    (check (vector-ref fu-count 0) => 2)
    ;; 消费顺序：steering 先于 follow-up
    (check (user-texts-of res-msgs) => '("hi" "改这里" "follow-up 后到"))
  ) ;let
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 3: error 为硬退出：不轮询 follow-up，直接结算
;; -----------------------------------------------------------------------------
(let* ((req-count (vector 0))
       (request-fn
         (lambda (provider config msgs sys-prompt tools on-ev timeout)
           (vector-set! req-count 0 (+ 1 (vector-ref req-count 0)))
           (make-error-response)
         ) ;lambda
        )
       (fu-count (vector 0))
       (get-follow-up
         (lambda ()
           (vector-set! fu-count 0 (+ 1 (vector-ref fu-count 0)))
           '("不该被消费")
         ) ;lambda
        )
       (emit (make-emit-collector))
       (cfg (list (cons 'provider "test")
              (cons 'tools '())
              (cons 'settings '())
              (cons 'get-follow-up-messages get-follow-up)
            ) ;list
           )
        ) ;
  (let ((res-msgs (run-agent-loop cfg (list (pi-user-message "hi")) "sys" emit 10 5
                    600 #f request-fn
                  ) ;run-agent-loop
               ) ;res-msgs
           ) ;let
    (check (vector-ref req-count 0) => 1)
    (check (vector-ref fu-count 0) => 0)
    (check (length res-msgs) => 2)
  ) ;let
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 4: 工具批次 terminate 后自然停止：follow-up 仍被消费并续跑
;; （对齐上游：terminate 只跳过工具续跑，不阻断 follow-up）
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
       (exec
         (lambda (name . args)
           (list (cons 'content
                  (list (list (cons 'type "text") (cons 'text "done working")))
                ) ;list
             (cons 'isError #f)
           ) ;list
         ) ;lambda
        )
       ;; 经扩展事件阻断并 terminate：正常工具结果不携带 terminate，
       ;; 唯一触发途径是 emit-extension-event 返回 block+terminate
       (ext-emit
         (lambda (ev-type payload)
           (if (eq? ev-type 'tool_call)
             '((action . block) (reason . "enough") (terminate . #t))
             '((action . continue))
           ) ;if
         ) ;lambda
        )
       (fu-count (vector 0))
       (get-follow-up
         (lambda ()
           (vector-set! fu-count 0 (+ 1 (vector-ref fu-count 0)))
           (if (= (vector-ref fu-count 0) 1) '("收尾说明") '())
         ) ;lambda
        )
       (emit (make-emit-collector))
       (cfg (list (cons 'provider "test")
              (cons 'tools '("bash"))
              (cons 'settings '())
              (cons 'execute-tool-call exec)
              (cons 'emit-extension-event ext-emit)
              (cons 'get-follow-up-messages get-follow-up)
            ) ;list
           )
        ) ;
  (let ((res-msgs (run-agent-loop cfg (list (pi-user-message "hi")) "sys" emit 10 5
                    600 #f request-fn
                  ) ;run-agent-loop
               ) ;res-msgs
           ) ;let
    (check (vector-ref req-count 0) => 2)
    ;; 第 2 轮上下文：user → assistant → toolResult → user(follow-up)
    (check (roles-of (vector-ref last-req-msgs 0))
      => '("user" "assistant" "toolResult" "user")
    ) ;check
    (check (user-texts-of res-msgs) => '("hi" "收尾说明"))
  ) ;let
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 5: follow-up 回调抛异常时按无消息处理，循环正常结束
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
              (cons 'get-follow-up-messages (lambda () (error 'boom "fu failed")))
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
