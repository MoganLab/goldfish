;; pi-event-session-type
;; 返回 session 事件的固定类型字符串 "session"。
;;
;; 语法
;; ----
;; (pi-event-session-type ev)
;;
;; 参数
;; ----
;; ev : any?
;; 事件对象。
;;
;; 返回值
;; ----
;; string?
;; 字符串 "session"。

(import (liii check) (liii pi ai event record))

(check-set-mode! 'report-failed)

(check (pi-event-session-type 'dummy) => "session")

(check-report)
