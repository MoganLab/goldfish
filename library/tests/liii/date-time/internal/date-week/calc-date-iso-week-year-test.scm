;; calc-date-iso-week-year
;; 计算日期所属的 ISO 周年份。
;;
;; 语法
;; ----
;; (calc-date-iso-week-year date)
;;
;; 参数
;; ----
;; date : date? 日期对象。
;;
;; 返回值
;; ----
;; integer? ISO 年份，可能与日历年不同。

(import (liii check) (liii date-time date) (liii date-time internal date-week))

(check-set-mode! 'report-failed)

(check (calc-date-iso-week-year (make-date 2008 12 29)) => 2009)
(check (calc-date-iso-week-year (make-date 2010 1 2)) => 2009)
(check (calc-date-iso-week-year (make-date 2021 1 1)) => 2020)
(check (calc-date-iso-week-year (make-date 2021 6 15)) => 2021)
(check-catch 'type-error (calc-date-iso-week-year "date"))

(check-report)
