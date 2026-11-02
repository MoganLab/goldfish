;; pi-event-turn-end-type
;; 返回 turn-end 事件的固定类型字符串 "turn_end"。
;;
;; 语法
;; ----
;; (pi-event-turn-end-type ev)
;;
;; 参数
;; ----
;; ev : any?
;; 事件对象。
;;
;; 返回值
;; ----
;; string?
;; 字符串 "turn_end"。

(import (liii check) (liii pi ai event record))

(check-set-mode! 'report-failed)

(check (pi-event-turn-end-type 'dummy) => "turn_end")

(check-report)
