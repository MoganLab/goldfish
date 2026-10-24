(import (liii check) (liii pi ai event) (liii pi ai message))

(check-set-mode! 'report-failed)

;; pi-event-agent-start
;; 构造 agent_start 事件对象。
(let ((ev (pi-event-agent-start)))
  (check-true (pi-event-agent-start? ev))
  (check (pi-event-agent-start-type ev) => "agent_start")
  ;; 兼容 alist 访问
  (check (cdr (assoc "type" ev)) => "agent_start"))

(check-report)
