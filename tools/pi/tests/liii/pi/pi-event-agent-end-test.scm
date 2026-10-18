(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-event-agent-end
;; 构造 agent_end 会话结束事件对象。
(let ((ev (pi-event-agent-end '() #f)))
  (check (cdr (assoc "type" ev)) => "agent_end"))

(check-report)
