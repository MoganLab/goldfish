;; tz-month-week-day:month
;; 读取月-周-星期记录的月份字段。
;;
;; 语法
;; ----
;; tz-month-week-day:month mwd
;;
;; 参数
;; ----
;; mwd : tz-month-week-day? 日期规则记录。
;;
;; 返回值
;; ----
;; integer? 月份。

(import (liii check) (liii date-time internal tz-types))

(check-set-mode! 'report-failed)

(define mwd (tz-month-week-day 3 2 0))

(check (tz-month-week-day:month mwd) => 3)

(check-report)
