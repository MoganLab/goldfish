;; pi-event-message-update-type
;; 返回 message-update 事件的固定类型字符串 "message_update"。
;;
;; 语法
;; ----
;; (pi-event-message-update-type ev)
;;
;; 参数
;; ----
;; ev : any?
;; 事件对象。
;;
;; 返回值
;; ----
;; string?
;; 字符串 "message_update"。

(import (liii check) (liii pi ai event record))

(check-set-mode! 'report-failed)

(check (pi-event-message-update-type 'dummy) => "message_update")

(check-report)
