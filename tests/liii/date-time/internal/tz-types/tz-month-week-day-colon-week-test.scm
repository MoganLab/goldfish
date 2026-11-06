;; tz-month-week-day:week
;; 读取月-周-星期记录的周数字段。
;;
;; 语法
;; ----
;; tz-month-week-day:week mwd
;;
;; 参数
;; ----
;; mwd : tz-month-week-day? 日期规则记录。
;;
;; 返回值
;; ----
;; integer? 第几周。

(import (liii check) (liii date-time internal tz-types))

(check-set-mode! 'report-failed)

(define mwd (tz-month-week-day 3 2 0))

(check (tz-month-week-day:week mwd) => 2)

(check-report)
