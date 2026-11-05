;; pi-event-turn-start-type
;; 返回 turn-start 事件的固定类型字符串 "turn_start"。
;;
;; 语法
;; ----
;; (pi-event-turn-start-type ev)
;;
;; 参数
;; ----
;; ev : any?
;; 事件对象。
;;
;; 返回值
;; ----
;; string?
;; 字符串 "turn_start"。

(import (liii check) (liii pi ai event record))

(check-set-mode! 'report-failed)

(check (pi-event-turn-start-type 'dummy) => "turn_start")

(check-report)
