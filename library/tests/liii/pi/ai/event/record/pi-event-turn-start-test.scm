;; pi-event-turn-start
;; 构造 turn_start 事件对象。
;;
;; 语法
;; ----
;; (pi-event-turn-start)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; pi-event-turn-start?
;; 回合开始事件 record。

(import (liii check) (liii pi ai event record))

(check-set-mode! 'report-failed)

(let ((ev (pi-event-turn-start)))
  (check-true (pi-event-turn-start? ev))
  (check (pi-event-turn-start-type ev) => "turn_start")
) ;let

(check-report)
