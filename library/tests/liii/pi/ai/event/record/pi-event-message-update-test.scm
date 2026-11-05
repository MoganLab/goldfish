;; pi-event-message-update
;; 构造 message_update 增量事件对象。
;;
;; 语法
;; ----
;; (pi-event-message-update usage assistant-message-event)
;;
;; 参数
;; ----
;; usage : any?
;; 用量统计（或空列表/哈希表）。
;;
;; assistant-message-event : any?
;; 增量消息事件。
;;
;; 返回值
;; ----
;; pi-event-message-update?
;; 消息增量事件 record。

(import (liii check) (liii pi ai event record))

(check-set-mode! 'report-failed)

(let ((ev (pi-event-message-update '() '(("type" . "text_delta")))))
  (check-true (pi-event-message-update? ev))
  (check (pi-event-message-update-type ev) => "message_update")
  (check (pi-event-message-update-assistant-message-event ev)
    =>
    '(("type" . "text_delta"))
  ) ;check
) ;let

(check-report)
