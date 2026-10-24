(import (liii check) (liii pi ai event) (liii pi ai message))

(check-set-mode! 'report-failed)

;; pi-event-agent-settled
;; 构造 agent_settled 会话完成结算事件对象。
(let ((ev (pi-event-agent-settled #f)))
  (check-true (pi-event-agent-settled? ev))
  (check (pi-event-agent-settled-type ev) => "agent_settled")
  (check (pi-event-agent-settled-aborted ev) => #f)
  ;; 兼容 alist 访问
  (check (cdr (assoc "type" ev)) => "agent_settled")
  (check (cdr (assoc "aborted" ev)) => #f))

(check-report)
