;; pi-step-agent-turn
;; 执行单步 Agent 轮次：调用模型请求函数、附加开销计算并提取工具调用与停止原因。
;;
;; 语法
;; ----
;; (pi-step-agent-turn config msgs system-prompt emit timeout request-fn)
;;
;; 参数
;; ----
;; config : list?，包含 tools、provider 等配置的 alist。
;; msgs : list?，当前会话消息列表。
;; system-prompt : string?，当前轮次的系统提示词。
;; emit : (or procedure? #f)，事件派发过程。
;; timeout : real?，请求超时时间（秒）。
;; request-fn : procedure?，模型请求派发过程。
;;
;; 返回值
;; ----
;; (values list? string? list? any?)，返回更新后的消息列表、停止原因、工具调用列表以及响应对象。

(import (scheme base) (liii check) (liii string) (liii pi ai message) (liii pi agent loop))

(check-set-mode! 'report-failed)

;; 1. 模拟模型返回普通文本响应（stop-reason 为 "stop"）
(let* ((evs '())
       (emit (lambda (ev-type . args) (set! evs (cons ev-type evs))))
       (mock-req
         (lambda (provider cfg msgs sp tools on-stream to)
           (pi-assistant-message (list (list (cons 'type "text") (cons 'text "Hello user")))
             :stop-reason "stop"
             :usage       '((input . 10) (output . 5) (total . 15))
           ) ;pi-assistant-message
         ) ;lambda
       ) ;mock-req
       (cfg '((tools) (provider . "google") (model-id . "gemini-3.8-flash")))
       (init-msgs (list (pi-user-message "Hi")))
      ) ;
  (let-values (((msgs stop-reason tool-calls resp)
                (pi-step-agent-turn cfg init-msgs "prompt" emit 10 mock-req)
               ) ;
              ) ;
    (check (length msgs) => 2)
    (check stop-reason => "stop")
    (check tool-calls => '())
    (check-true (pi-assistant-message? resp))
    (check-true (pair? (member 'message_start evs)))
    (check-true (pair? (member 'message_end evs)))
  ) ;let-values
) ;let*

;; 2. 模拟模型返回包含 toolCall 的响应（stop-reason 为 "toolUse"）
(let* ((mock-req
         (lambda (provider cfg msgs sp tools on-stream to)
           '((role . "assistant")
             (stopReason . "toolUse")
             (content ((type . "toolCall")
                       (id . "c_1")
                       (name . "bash")
                       (arguments))))
         ) ;lambda
       ) ;mock-req
       (cfg '((tools) (provider . "google")))
      ) ;
  (let-values (((msgs stop-reason tool-calls resp)
                (pi-step-agent-turn cfg '() "prompt" #f 10 mock-req)
               ) ;
              ) ;
    (check (length msgs) => 1)
    (check stop-reason => "toolUse")
    (check (length tool-calls) => 1)
    (check (cdr (assoc 'id (car tool-calls))) => "c_1")
  ) ;let-values
) ;let*

;; 3. 模拟请求超时时重试并包含超时秒数提示
(let* ((retry-msgs '())
       (emit (lambda (ev-type . args)
               (when (eq? ev-type 'retry)
                 (set! retry-msgs (cons (car args) retry-msgs))
               ) ;when
             ) ;lambda
       ) ;emit
       (attempts 0)
       (mock-req
         (lambda (provider cfg msgs sp tools on-stream to)
           (set! attempts (+ attempts 1))
           (if (= attempts 1)
             (pi-assistant-message '()
               :stop-reason "error"
               :error-message "Request timed out after 15s"
             ) ;pi-assistant-message
             (pi-assistant-message (list (list (cons 'type "text") (cons 'text "Recovered!")))
               :stop-reason "stop"
             ) ;pi-assistant-message
           ) ;if
         ) ;lambda
       ) ;mock-req
       (cfg '((tools) (provider . "google")
              (settings . ((retry . ((enabled . #t) (max-retries . 2) (base-delay-ms . 10)))))))
      ) ;
  (let-values (((msgs stop-reason tool-calls resp)
                (pi-step-agent-turn cfg '() "prompt" emit 15 mock-req)
               ) ;
              ) ;
    (check attempts => 2)
    (check stop-reason => "stop")
    (check (length retry-msgs) => 1)
    (check-true (string-contains? (car retry-msgs) "Request timed out after 15s"))
    (check-true (string-contains? (car retry-msgs) "attempt 1/2"))
  ) ;let-values
) ;let*

;; 4. 模拟请求抛出 http-client-timeout-error 异常时被捕获并重试
(let* ((retry-msgs '())
       (emit (lambda (ev-type . args)
               (when (eq? ev-type 'retry)
                 (set! retry-msgs (cons (car args) retry-msgs))
               ) ;when
             ) ;lambda
       ) ;emit
       (attempts 0)
       (mock-req
         (lambda (provider cfg msgs sp tools on-stream to)
           (set! attempts (+ attempts 1))
           (if (= attempts 1)
             (error 'http-client-timeout-error "Operation timed out")
             (pi-assistant-message (list (list (cons 'type "text") (cons 'text "Success")))
               :stop-reason "stop"
             ) ;pi-assistant-message
           ) ;if
         ) ;lambda
       ) ;mock-req
       (cfg '((tools) (provider . "google")
              (settings . ((retry . ((enabled . #t) (max-retries . 2) (base-delay-ms . 10)))))))
      ) ;
  (let-values (((msgs stop-reason tool-calls resp)
                (pi-step-agent-turn cfg '() "prompt" emit 20 mock-req)
               ) ;
              ) ;
    (check attempts => 2)
    (check stop-reason => "stop")
    (check (length retry-msgs) => 1)
    (check-true (string-contains? (car retry-msgs) "Request timed out after 20s"))
  ) ;let-values
) ;let*

(check-report)
