;; calc-date-iso-week
;; 计算日期所在的 ISO 周数。
;;
;; 语法
;; ----
;; (calc-date-iso-week date)
;;
;; 参数
;; ----
;; date : date? 日期对象。
;;
;; 返回值
;; ----
;; integer? ISO 周数（1-53）。

(import (liii check) (liii date-time date) (liii date-time internal date-week))

(check-set-mode! 'report-failed)

(check (calc-date-iso-week (make-date 2025 10 14)) => 42)
(check (calc-date-iso-week (make-date 2008 12 29)) => 1)
(check (calc-date-iso-week (make-date 2010 1 2)) => 53)
(check (calc-date-iso-week (make-date 2021 1 1)) => 53)
(check-catch 'type-error (calc-date-iso-week "date"))

(check-report)
