(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-event-message-update
;; 构造 message_update 流式增量事件对象。
(let* ((usage '(("input" . 10) ("output" . 5) ("total" . 15)))
       (delta '(("type" . "text_delta") ("delta" . "hello")))
       (ev (pi-event-message-update usage delta)))
  (check-true (pi-event-message-update? ev))
  (check (pi-event-message-update-type ev) => "message_update")
  (check (pi-event-message-update-usage ev) => usage)
  (check (pi-event-message-update-assistant-message-event ev) => delta)
  ;; 兼容 alist 访问
  (check (cdr (assoc "type" ev)) => "message_update")
  (check (cdr (assoc "usage" ev)) => usage)
  (check (cdr (assoc "assistantMessageEvent" ev)) => delta))

(check-report)
