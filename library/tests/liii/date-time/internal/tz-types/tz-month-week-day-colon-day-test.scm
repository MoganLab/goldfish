;; tz-month-week-day:day
;; 读取月-周-星期记录的星期字段。
;;
;; 语法
;; ----
;; tz-month-week-day:day mwd
;;
;; 参数
;; ----
;; mwd : tz-month-week-day? 日期规则记录。
;;
;; 返回值
;; ----
;; integer? 星期编号。

(import (liii check) (liii date-time internal tz-types))

(check-set-mode! 'report-failed)

(define mwd (tz-month-week-day 3 2 0))

(check (tz-month-week-day:day mwd) => 0)

(check-report)
