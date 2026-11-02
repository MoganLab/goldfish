;; pi-event-message-start-type
;; 返回 message-start 事件的固定类型字符串 "message_start"。
;;
;; 语法
;; ----
;; (pi-event-message-start-type ev)
;;
;; 参数
;; ----
;; ev : any?
;; 事件对象。
;;
;; 返回值
;; ----
;; string?
;; 字符串 "message_start"。

(import (liii check) (liii pi ai event record))

(check-set-mode! 'report-failed)

(check (pi-event-message-start-type 'dummy) => "message_start")

(check-report)
