;; json->pi-agent-event
;; 将 JSON 对象转换为对应的 Agent 事件 record。
;;
;; 语法
;; ----
;; (json->pi-agent-event obj)
;;
;; 参数
;; ----
;; obj : list? | pi-agent-event?
;; JSON 对象 alist 或 Agent 事件 record。
;;
;; 返回值
;; ----
;; pi-agent-event? | #f
;; 对应的 Agent 事件 record；未知类型返回 #f。

(import (liii check) (liii pi agent event))

(check-set-mode! 'report-failed)

(let ((ev (json->pi-agent-event '(("type" . "agent_start")))))
  (check-true (pi-event-agent-start? ev))
) ;let
(check-false (json->pi-agent-event '(("type" . "unknown"))))

(check-report)
