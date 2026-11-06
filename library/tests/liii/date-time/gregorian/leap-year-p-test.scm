(import (liii check)
        (liii date-time gregorian))

(check-set-mode! 'report-failed)

;; leap-year?
;; 判断给定年份在公历中是否为闰年。
;;
;; 语法
;; ----
;; (leap-year? year)
;;
;; 参数
;; ----
;; year : integer? 年份。
;;
;; 返回值
;; ----
;; boolean? 如果为闰年返回 #t，否则返回 #f。
;;
;; 示例
;; ----
;; (leap-year? 2020) => #t
;; (leap-year? 1900) => #f
;; (leap-year? 2000) => #t
;;
;; 错误处理
;; --------
;; type-error 当参数不是整数时抛出错误。

(check-true (leap-year? 2020))
(check-true (leap-year? 2000))
(check-true (leap-year? 4))
(check-true (leap-year? 0))
(check-true (leap-year? -4))
(check-true (leap-year? -400))
(check-false (leap-year? 1900))
(check-false (leap-year? 2021))
(check-false (leap-year? 2100))
(check-false (leap-year? -1))

(check-catch 'type-error (leap-year? 2020.5))
(check-catch 'type-error (leap-year? "2020"))
(check-catch 'type-error (leap-year? 'foo))

(check-report)
