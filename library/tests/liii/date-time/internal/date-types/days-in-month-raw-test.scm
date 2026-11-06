;; days-in-month-raw
;; 返回给定年份类型与月份的天数，非法月份返回 #f。
;;
;; 语法
;; ----
;; (days-in-month-raw is-leap-year month)
;;
;; 参数
;; ----
;; is-leap-year : boolean? 是否为闰年。
;; month : integer? 月份。
;;
;; 返回值
;; ----
;; integer? 该月天数；month 非法时返回 #f。

(import (liii check) (liii date-time internal date-types))

(check-set-mode! 'report-failed)

(check (days-in-month-raw #f 1) => 31)
(check (days-in-month-raw #f 4) => 30)
(check (days-in-month-raw #f 2) => 28)
(check (days-in-month-raw #t 2) => 29)
(check (days-in-month-raw #f 12) => 31)
(check (days-in-month-raw #f 13) => #f)
(check (days-in-month-raw #f 0) => #f)

(check-report)
