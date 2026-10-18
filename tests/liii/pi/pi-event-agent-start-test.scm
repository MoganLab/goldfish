(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-event-agent-start
;; 构造 agent_start 事件对象。
(let ((ev (pi-event-agent-start)))
  (check (cdr (assoc "type" ev)) => "agent_start"))

(check-report)
