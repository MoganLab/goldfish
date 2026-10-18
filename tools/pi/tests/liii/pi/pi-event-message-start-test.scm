(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-event-message-start
;; 构造 message_start 事件对象。
(let* ((msg '(("role" . "user") ("content" . "hi")))
       (ev (pi-event-message-start msg)))
  (check-true (pi-event-message-start? ev))
  (check (pi-event-message-start-type ev) => "message_start")
  (check (pi-event-message-start-message ev) => msg)
  ;; 兼容 alist 访问
  (check (cdr (assoc "type" ev)) => "message_start")
  (check (cdr (assoc "message" ev)) => msg))

(check-report)
