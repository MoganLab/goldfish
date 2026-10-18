(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-event-message-end
;; 构造 message_end 事件对象。
(let ((ev (pi-event-message-end '(("role" . "assistant")))))
  (check (cdr (assoc "type" ev)) => "message_end"))

(check-report)
