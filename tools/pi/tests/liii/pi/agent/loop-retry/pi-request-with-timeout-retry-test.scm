;; pi-request-with-timeout-retry
;; 执行模型请求，遇到超时时自动重试，并在重试提示信息中明确体现超时秒数。
;;
;; 语法
;; ----
;; (pi-request-with-timeout-retry req-fn prov cfg msgs sp tools on-ev to emit)
;;
;; 参数
;; ----
;; req-fn : procedure?，底层请求派发过程。
;; prov : string?，模型供应商名称。
;; cfg : list?，包含 settings、retry 等配置的 alist。
;; msgs : list?，当前会话消息列表。
;; sp : string?，系统提示词。
;; tools : list?，激活工具列表。
;; on-ev : (or procedure? #f)，流式更新回调。
;; to : real?，超时时限（秒）。
;; emit : (or procedure? #f)，事件派发过程。
;;
;; 返回值
;; ----
;; any?，返回模型响应对象。

(import (scheme base)
  (liii check)
  (liii string)
  (liii pi ai message)
  (liii pi agent loop-retry)
) ;import

(check-set-mode! 'report-failed)

;; 1. 模拟非超时正常请求直接返回
(let* ((mock-req (lambda (prov cfg msgs sp tools on-ev to)
                   (pi-assistant-message (list (list (cons 'type "text") (cons 'text "OK")))
                     :stop-reason "stop"
                   ) ;pi-assistant-message
                 ) ;lambda
       ) ;mock-req
       (res (pi-request-with-timeout-retry mock-req "google" '() '() #f '() #f 10 #f))
      ) ;
  (check (pi-assistant-message? res) => #t)
  (check (pi-assistant-message-stop-reason res) => "stop")
) ;let*

;; 2. 模拟超时并在重试提示中明确体现秒数
(let* ((retry-msgs '())
       (emit (lambda (ev-type . args)
               (when (eq? ev-type 'retry)
                 (set! retry-msgs (cons (car args) retry-msgs))
               ) ;when
             ) ;lambda
       ) ;emit
       (attempts 0)
       (mock-req (lambda (prov cfg msgs sp tools on-ev to)
                   (set! attempts (+ attempts 1))
                   (if (= attempts 1)
                     (pi-assistant-message '()
                       :stop-reason "error"
                       :error-message "Request timed out after 30s"
                     ) ;pi-assistant-message
                     (pi-assistant-message (list (list (cons 'type "text") (cons 'text "Success")))
                       :stop-reason "stop"
                     ) ;pi-assistant-message
                   ) ;if
                 ) ;lambda
       ) ;mock-req
       (cfg '((settings . ((retry . ((enabled . #t) (max-retries . 2) (base-delay-ms . 10)))))))
       (res (pi-request-with-timeout-retry mock-req "google" cfg '() #f '() #f 30 emit))
      ) ;
  (check attempts => 2)
  (check (pi-assistant-message-stop-reason res) => "stop")
  (check (length retry-msgs) => 1)
  (check-true (string-contains? (car retry-msgs) "Request timed out after 30s"))
) ;let*

(check-report)
