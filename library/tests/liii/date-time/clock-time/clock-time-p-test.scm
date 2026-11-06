(import (liii check)
        (liii date-time clock-time))

(check-set-mode! 'report-failed)

;; clock-time?
;; 判断给定对象是否为 clock-time 对象。
;;
;; 语法
;; ----
;; (clock-time? obj)
;;
;; 参数
;; ----
;; obj : any
;;
;; 返回值
;; ----
;; boolean? 若为 clock-time 返回 #t，否则返回 #f。
;;
;; 示例
;; ----
;; (clock-time? (make-clock-time 12 0 0)) => #t
;; (clock-time? "12:00:00") => #f

(check-true (clock-time? (make-clock-time 12 0 0)))
(check-true (clock-time? (make-clock-time 0 0 0)))
(check-false (clock-time? "12:00:00"))
(check-false (clock-time? 120000))
(check-false (clock-time? #f))
(check-false (clock-time? '(12 0 0)))

(check-report)
