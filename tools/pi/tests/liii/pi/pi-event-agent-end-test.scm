(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-event-agent-end
;; 构造 agent_end 会话结束事件对象。
(let* ((msgs '())
       (ev (pi-event-agent-end msgs #f)))
  (check-true (pi-event-agent-end? ev))
  (check (pi-event-agent-end-type ev) => "agent_end")
  (check (pi-event-agent-end-messages ev) => msgs)
  (check (pi-event-agent-end-will-retry ev) => #f)
  ;; 兼容 alist 访问
  (check (cdr (assoc "type" ev)) => "agent_end")
  (check (cdr (assoc "messages" ev)) => msgs)
  (check (cdr (assoc "willRetry" ev)) => #f))

(check-report)
