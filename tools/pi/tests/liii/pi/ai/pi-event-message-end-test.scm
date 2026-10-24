(import (liii check) (liii pi ai event) (liii pi ai message))

(check-set-mode! 'report-failed)

;; pi-event-message-end
;; 构造 message_end 事件对象。
(let* ((msg '(("role" . "assistant") ("content" . "bye")))
       (ev (pi-event-message-end msg)))
  (check-true (pi-event-message-end? ev))
  (check (pi-event-message-end-type ev) => "message_end")
  (check (pi-event-message-end-message ev) => msg)
  ;; 兼容 alist 访问
  (check (cdr (assoc "type" ev)) => "message_end")
  (check (cdr (assoc "message" ev)) => msg))

(check-report)
