;; days-since-year-start
;; 计算日期距离当年 1 月 1 日的天数（1-based）。
;;
;; 语法
;; ----
;; (days-since-year-start date)
;;
;; 参数
;; ----
;; date : date? 日期对象。
;;
;; 返回值
;; ----
;; integer? 年内第几天。

(import (liii check) (liii date-time date) (liii date-time internal date-rd))

(check-set-mode! 'report-failed)

(check (days-since-year-start (make-date 2021 1 1)) => 1)
(check (days-since-year-start (make-date 2021 3 1)) => 60)
(check (days-since-year-start (make-date 2020 3 1)) => 61)
(check (days-since-year-start (make-date 2021 12 31)) => 365)
(check-catch 'type-error (days-since-year-start "2021-01-01"))

(check-report)
