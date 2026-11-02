;; pi-finish-agent-turn
;; 结束单轮对话执行：处理因长度截断的未完成工具调用，派发轮次与会话结束事件并返回最终消息列表。
;;
;; 语法
;; ----
;; (pi-finish-agent-turn new-msgs stop-reason tool-calls emit config resp)
;;
;; 参数
;; ----
;; new-msgs : list?，当前追加了响应的消息列表。
;; stop-reason : string?，停止原因（如 "stop"、"length"）。
;; tool-calls : list?，本轮工具调用列表。
;; emit : (or procedure? #f)，事件派发过程。
;; config : list?，Agent 配置 alist。
;; resp : any?，本轮 assistant 响应消息对象。
;;
;; 返回值
;; ----
;; list?，处理完毕后的最终会话消息列表。

(import (scheme base) (liii check) (liii pi ai message) (liii pi agent loop))

(check-set-mode! 'report-failed)

;; 1. 正常停止（stop-reason 为 "stop"）：无多余工具追加，派发 turn_end 和 agent_end
(let* ((evs '())
       (emit (lambda (ev-type . args) (set! evs (cons ev-type evs))))
       (resp (pi-assistant-message "hello" :stop-reason "stop"))
       (msgs (list resp))
       (cfg '((provider . "google")
              (model-id . "gemini-3.8-flash")
              (models-store)))
       (final-msgs (pi-finish-agent-turn msgs "stop" '() emit cfg resp))
      ) ;
  (check (length final-msgs) => 1)
  (check-true (pair? (member 'turn_end evs)))
  (check-true (pair? (member 'agent_end evs)))
) ;let*

;; 2. 长度截断（stop-reason 为 "length"）且包含工具调用：自动追加失败工具结果
(let* ((resp (pi-assistant-message '() :stop-reason "length"))
       (msgs (list resp))
       (calls (list '((id . "c1") (name . "bash"))))
       (cfg '((provider . "google")
              (model-id . "gemini-3.8-flash")
              (models-store)))
       (final-msgs (pi-finish-agent-turn msgs "length" calls #f cfg resp))
      ) ;
  ;; msgs 基础上追加了 1 条失败的 toolResult
  (check (length final-msgs) => 2)
  (check-true (pi-tool-result-message? (cadr final-msgs)))
  (check (pi-tool-result-message-is-error (cadr final-msgs)) => #t)
) ;let*

(check-report)
