(import (liii check)
        (liii time)
        (srfi srfi-19)
) ;import

(check-set-mode! 'report-failed)

;; date-nanosecond date-second date-minute date-hour
;; date-day date-month date-year date-zone-offset
;; 获取日期对象的组成部分。
;;
;; 语法
;; ----
;; (date-nanosecond date)
;; (date-second date)
;; (date-minute date)
;; (date-hour date)
;; (date-day date)
;; (date-month date)
;; (date-year date)
;; (date-zone-offset date)
;;
;; 参数
;; ----
;; date : date? 日期对象。
;;
;; 返回值
;; ----
;; 各部分的整数值。
;;
;; 错误处理
;; --------
;; wrong-type-arg 当参数不是日期对象时抛出错误。

;; Test date accessors
(let ((d (make-date 123456789 45 30 14 25 12 2023 28800)))
  (check (date-nanosecond d) => 123456789)
  (check (date-second d) => 45)
  (check (date-minute d) => 30)
  (check (date-hour d) => 14)
  (check (date-day d) => 25)
  (check (date-month d) => 12)
  (check (date-year d) => 2023)
  (check (date-zone-offset d) => 28800)
) ;let

;; Test with different values
(let ((d (make-date 999999999 59 59 23 31 1 2000 -14400)))
  (check (date-nanosecond d) => 999999999)
  (check (date-second d) => 59)
  (check (date-minute d) => 59)
  (check (date-hour d) => 23)
  (check (date-day d) => 31)
  (check (date-month d) => 1)
  (check (date-year d) => 2000)
  (check (date-zone-offset d) => -14400)
) ;let

;; Test error conditions
(check-catch 'wrong-type-arg (date-nanosecond "not-a-date"))
(check-catch 'wrong-type-arg (date-second 123))
(check-catch 'wrong-type-arg (date-minute 'symbol))
(check-catch 'wrong-type-arg (date-hour #t))
(check-catch 'wrong-type-arg (date-day #(vector)))
(check-catch 'wrong-type-arg (date-month (cons 1 2)))
;; FIXME: strange?
(check-true  (undefined? (date-year (make-time TIME-UTC 0 0))))
(check-catch 'wrong-type-arg (date-zone-offset #f))

(check-report)
