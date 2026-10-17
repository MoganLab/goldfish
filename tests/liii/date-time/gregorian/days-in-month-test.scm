(import (liii check)
        (liii date-time gregorian))

(check-set-mode! 'report-failed)

;; days-in-month
;; 获取公历某年某月的天数。
;;
;; 语法
;; ----
;; (days-in-month year month)
;;
;; 参数
;; ----
;; year : integer? 年份。
;; month : integer? 月份（1-12）。
;;
;; 返回值
;; ----
;; integer? 该月天数（28, 29, 30 或 31）。
;;
;; 示例
;; ----
;; (days-in-month 2020 2) => 29
;; (days-in-month 2021 2) => 28
;;
;; 错误处理
;; --------
;; type-error 当参数不是整数或月份超出 1-12 范围时抛出错误。

(check (days-in-month 2020 2) => 29)
(check (days-in-month 2021 2) => 28)
(check (days-in-month 1900 2) => 28)
(check (days-in-month 2000 2) => 29)
(check (days-in-month 2021 1) => 31)
(check (days-in-month 2021 3) => 31)
(check (days-in-month 2021 4) => 30)
(check (days-in-month 2021 5) => 31)
(check (days-in-month 2021 6) => 30)
(check (days-in-month 2021 7) => 31)
(check (days-in-month 2021 8) => 31)
(check (days-in-month 2021 9) => 30)
(check (days-in-month 2021 10) => 31)
(check (days-in-month 2021 11) => 30)
(check (days-in-month 2021 12) => 31)

(check-catch 'type-error (days-in-month 2021 0))
(check-catch 'type-error (days-in-month 2021 13))
(check-catch 'type-error (days-in-month "2021" 1))
(check-catch 'type-error (days-in-month 2021 1.5))
(check-catch 'type-error (days-in-month 'foo 1))

(check-report)
