;; calc-date-iso-weekday
;; 计算日期的 ISO 星期（1=周一 到 7=周日）。
;;
;; 语法
;; ----
;; (calc-date-iso-weekday date)
;;
;; 参数
;; ----
;; date : date? 日期对象。
;;
;; 返回值
;; ----
;; integer? ISO 星期编号（1-7）。

(import (liii check) (liii date-time date) (liii date-time internal date-week))

(check-set-mode! 'report-failed)

(check (calc-date-iso-weekday (make-date 2021 1 4)) => 1)
(check (calc-date-iso-weekday (make-date 2021 1 3)) => 7)
(check (calc-date-iso-weekday (make-date 2025 10 14)) => 2)
(check-catch 'type-error (calc-date-iso-weekday "date"))

(check-report)
