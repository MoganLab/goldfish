;; pi-event-turn-start?
;; 判断对象是否为单轮回合开始事件 record 实例。
;;
;; 语法
;; ----
;; (pi-event-turn-start? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 若属于单轮回合开始事件 record 实例则返回 #t，否则返回 #f。

(import (liii check) (liii pi ai event types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-event-turn-start)))
  (check-true (pi-event-turn-start? obj))
) ;let
(check-false (pi-event-turn-start? '()))
(check-false (pi-event-turn-start? #f))
(check-false (pi-event-turn-start? "not-a-record"))

(check-report)
