(import (scheme base)
  (liii go)
  (liii list)
  (liii check)
  (liii pi agent loop)
  (liii pi code modes interactive worker)
  (liii pi ai message)
) ;import

(check-set-mode! 'report-failed)

;; e2e 回归：TUI worker 侧轮询回调（%poll-one-injected，通道 → user_message
;; 事件 → 单元素列表）驱动 run-agent-loop 的 steering/follow-up 注入，
;; 验证跨线程通道与循环钩子的完整集成链路

(define (make-text-response text)
  (pi-assistant-message
    (list (list (cons 'type "text") (cons 'text text)))
    :stop-reason "stop"
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

(define (ok-exec name . args)
  (list (cons 'content
         (list (list (cons 'type "text") (cons 'text "tool output")))
       ) ;list
    (cons 'isError #f)
  ) ;list
) ;define

;; 测试项 1 的执行器：第 1 轮工具执行期间投递两条 steering（模拟主线程）
(define *steer-ch* #f)
(define (exec-and-steer name . args)
  (when *steer-ch*
    (chan-send! *steer-ch* "先看这个" 10)
    (chan-send! *steer-ch* "再看那个" 10)
  ) ;when
  (ok-exec name args)
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
;; 测试项 1: steering 通道注入：工具批次后经 %poll-one-injected 消费，
;; 回发 user_message 事件且消息进入下一轮请求上下文
;; -----------------------------------------------------------------------------
(let* ((steer-ch (make-chan 16))
       (event-ch (make-chan 32))
       (req-count (vector 0))
       (second-req-msgs (vector #f))
       (request-fn
         (lambda (provider config msgs sys-prompt tools on-ev timeout)
           (vector-set! req-count 0 (+ 1 (vector-ref req-count 0)))
           (when (= (vector-ref req-count 0) 2)
             (vector-set! second-req-msgs 0 msgs)
           ) ;when
           (if (= (vector-ref req-count 0) 1)
             (make-tool-use-response)
             (make-text-response "done")
           ) ;if
         ) ;lambda
        )
       (emit #f)
       (cfg (list (cons 'provider "test")
              (cons 'tools '("bash"))
              (cons 'settings '())
              (cons 'execute-tool-call exec-and-steer)
              (cons 'get-steering-messages
                (lambda () (%poll-one-injected steer-ch event-ch))
              )
            ) ;list
           )
        ) ;
  ;; 模拟主线程：第 1 轮工具执行期间投递两条 steering（见 exec-and-steer）
  (set! *steer-ch* steer-ch)
  (let ((res-msgs (run-agent-loop cfg (list (pi-user-message "hi")) "sys" emit 10 5
                    600 #f request-fn
                  ) ;run-agent-loop
               ) ;res-msgs
           ) ;let
    ;; 3 轮请求：工具轮 + 2 条 steering 各驱动一轮（one-at-a-time 语义）
    (check (vector-ref req-count 0) => 3)
    ;; 第 2 轮上下文：toolResult 之后紧跟第 1 条 steering（one-at-a-time：
    ;; 第 2 条在后续轮次注入）
    (check (roles-of (vector-ref second-req-msgs 0))
      => '("user" "assistant" "toolResult" "user")
    ) ;check
    (check (user-texts-of (vector-ref second-req-msgs 0)) => '("hi" "先看这个"))
    ;; 最终消息包含两条注入消息
    (check (user-texts-of res-msgs) => '("hi" "先看这个" "再看那个"))
    ;; event-chan 依次收到两条 user_message 事件（主线程据此同步显示）
    (check (chan-try-recv! event-ch #f) => '(user_message "先看这个"))
    (check (chan-try-recv! event-ch #f) => '(user_message "再看那个"))
    (check (chan-try-recv! event-ch #f) => #f)
    ;; steering 通道已排空
    (check (chan-try-recv! steer-ch #f) => #f)
  ) ;let
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 2: follow-up 通道注入：自然停止时消费，回发 user_message 事件
;; -----------------------------------------------------------------------------
(let* ((fu-ch (make-chan 16))
       (event-ch (make-chan 32))
       (req-count (vector 0))
       (request-fn
         (lambda (provider config msgs sys-prompt tools on-ev timeout)
           (vector-set! req-count 0 (+ 1 (vector-ref req-count 0)))
           (make-text-response "done")
         ) ;lambda
        )
       (emit #f)
       (cfg (list (cons 'provider "test")
              (cons 'tools '())
              (cons 'settings '())
              (cons 'get-follow-up-messages
                (lambda () (%poll-one-injected fu-ch event-ch))
              )
            ) ;list
           )
        ) ;
  (chan-send! fu-ch "结束后总结" 10)
  (let ((res-msgs (run-agent-loop cfg (list (pi-user-message "hi")) "sys" emit 10 5
                    600 #f request-fn
                  ) ;run-agent-loop
               ) ;res-msgs
           ) ;let
    (check (vector-ref req-count 0) => 2)
    (check (user-texts-of res-msgs) => '("hi" "结束后总结"))
    (check (chan-try-recv! event-ch #f) => '(user_message "结束后总结"))
    (check (chan-try-recv! fu-ch #f) => #f)
  ) ;let
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 3: 通道关闭/异常时回调返回空表，循环正常结束（防御性）
;; -----------------------------------------------------------------------------
(let* ((fu-ch (make-chan 1))
       (event-ch (make-chan 1))
       (req-count (vector 0))
       (request-fn
         (lambda (provider config msgs sys-prompt tools on-ev timeout)
           (vector-set! req-count 0 (+ 1 (vector-ref req-count 0)))
           (make-text-response "done")
         ) ;lambda
        )
       (emit #f)
       (cfg (list (cons 'provider "test")
              (cons 'tools '())
              (cons 'settings '())
              (cons 'get-follow-up-messages
                (lambda () (%poll-one-injected fu-ch event-ch))
              )
            ) ;list
           )
        ) ;
  ;; 关闭通道后轮询应安全返回空表
  (chan-close! fu-ch)
  (let ((res-msgs (run-agent-loop cfg (list (pi-user-message "hi")) "sys" emit 10 5
                    600 #f request-fn
                  ) ;run-agent-loop
               ) ;res-msgs
           ) ;let
    (check (vector-ref req-count 0) => 1)
    (check (user-texts-of res-msgs) => '("hi"))
  ) ;let
) ;let*

(check-report)
