;; pi-emit-agent-error
;; 派发 Agent 发生异常时的单轮事件流及最终的会话错误结束事件。
;;
;; 语法
;; ----
;; (pi-emit-agent-error err-msg msgs emit config)
;;
;; 参数
;; ----
;; err-msg : any?，错误响应消息对象（如包含 error-message 的 assistant 消息）。
;; msgs : list?，当前会话消息列表。
;; emit : (or procedure? #f)，事件派发过程。
;; config : list?，Agent 配置 alist。
;;
;; 返回值
;; ----
;; list?，追加了错误响应对象后的完整消息列表。

(import (scheme base) (liii check) (liii pi ai message) (liii pi agent loop))

(check-set-mode! 'report-failed)

;; 1. 触发完整的异常事件序列，并返回追加后的消息列表
(let* ((evs '())
       (emit (lambda (ev-type . args) (set! evs (cons ev-type evs))))
       (err-msg (pi-assistant-message '() :stop-reason "error" :error-message "network error")
       ) ;err-msg
       (msgs (list (pi-user-message "do something")))
       (cfg '((provider . "google")
              (model-id . "gemini-3.8-flash")
              (models-store)))
       (res (pi-emit-agent-error err-msg msgs emit cfg))
      ) ;
  (check (length res) => 2)
  (check (pi-assistant-message-error-message (cadr res)) => "network error")
  ;; 验证派发了 turn_start, message_start, message_end, turn_end, agent_end, agent_settled
  (check-true (pair? (member 'turn_start evs)))
  (check-true (pair? (member 'message_start evs)))
  (check-true (pair? (member 'message_end evs)))
  (check-true (pair? (member 'turn_end evs)))
  (check-true (pair? (member 'agent_end evs)))
  (check-true (pair? (member 'agent_settled evs)))
) ;let*

;; 2. emit 为 #f 时无事件派发，正常返回新消息列表
(let* ((err-msg (pi-assistant-message '() :stop-reason "error"))
       (res (pi-emit-agent-error err-msg '() #f '()))
      ) ;
  (check (length res) => 1)
) ;let*

(check-report)
