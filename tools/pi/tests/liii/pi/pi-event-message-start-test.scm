(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-event-message-start
;; 构造 message_start 事件对象。
(let ((ev (pi-event-message-start '(("role" . "user") ("content" . "hi")))))
  (check (cdr (assoc "type" ev)) => "message_start"))

(check-report)
