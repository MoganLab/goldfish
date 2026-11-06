;; tz-month-week-day?
;; 判断对象是否为月-周-星期记录。
;;
;; 语法
;; ----
;; tz-month-week-day? obj
;;
;; 参数
;; ----
;; obj : any? 任意对象。
;;
;; 返回值
;; ----
;; boolean? 是该类记录返回 #t。

(import (liii check) (liii date-time internal tz-types))

(check-set-mode! 'report-failed)

(define mwd (tz-month-week-day 3 2 0))

(check-true (tz-month-week-day? mwd))
(check-false (tz-month-week-day? "mwd"))

(check-report)
