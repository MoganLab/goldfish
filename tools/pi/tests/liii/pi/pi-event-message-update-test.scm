(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-event-message-update
;; 构造 message_update 流式增量事件对象。
(let ((ev (pi-event-message-update '() '(("type" . "text_delta") ("delta" . "hello")))))
  (check (cdr (assoc "type" ev)) => "message_update"))

(check-report)
