(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-event-agent-settled
;; 构造 agent_settled 会话完成结算事件对象。
(let ((ev (pi-event-agent-settled #f)))
  (check (cdr (assoc "type" ev)) => "agent_settled"))

(check-report)
