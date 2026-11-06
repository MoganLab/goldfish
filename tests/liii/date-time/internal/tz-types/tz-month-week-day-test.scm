;; tz-month-week-day
;; 构造 POSIX「月-周-星期」转移日期记录。
;;
;; 语法
;; ----
;; tz-month-week-day month week day
;;
;; 参数
;; ----
;; month : integer? 月份。
;; week : integer? 第几周（5 表示最后）。
;; day : integer? 星期编号。
;;
;; 返回值
;; ----
;; tz-month-week-day? 新建的日期规则记录。

(import (liii check) (liii date-time internal tz-types))

(check-set-mode! 'report-failed)

(define mwd (tz-month-week-day 3 2 0))

(check-true (tz-month-week-day? mwd))

(check-report)
