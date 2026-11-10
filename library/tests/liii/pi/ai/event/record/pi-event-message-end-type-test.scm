;; pi-event-message-end-type
;; 返回 message-end 事件的固定类型字符串 "message_end"。
;;
;; 语法
;; ----
;; (pi-event-message-end-type ev)
;;
;; 参数
;; ----
;; ev : any?
;; 事件对象。
;;
;; 返回值
;; ----
;; string?
;; 字符串 "message_end"。

(import (liii check) (liii pi ai event record))

(check-set-mode! 'report-failed)

(check (pi-event-message-end-type 'dummy) => "message_end")

(check-report)
