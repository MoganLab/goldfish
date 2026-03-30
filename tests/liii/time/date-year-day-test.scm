(import (liii check)
        (liii time)
        (srfi srfi-19)
) ;import

(check-set-mode! 'report-failed)

;; date-year-day
;; 获取日期在当年中的序号（1-365/366）。
;;
;; 语法
;; ----
;; (date-year-day date)
;;
;; 参数
;; ----
;; date : date? 日期对象。
;;
;; 返回值
;; ----
;; integer? 当年中的第几天，1 表示 1 月 1 日。
;;
;; 错误处理
;; --------
;; wrong-type-arg 当参数不是日期对象时抛出错误。
;; value-error 当日期的月份不合法时抛出错误。

;; Test date-year-day
(let ((d1 (make-date 0 0 0 0 1 1 2023 0))   ; non-leap year
      (d2 (make-date 0 0 0 0 1 3 2023 0))
      (d3 (make-date 0 0 0 0 1 3 2024 0))   ; leap year
      (d4 (make-date 0 0 0 0 31 12 2023 0))
      (d5 (make-date 0 0 0 0 31 12 2024 0)))    ; negative year
  (check (date-year-day d1) => 1)
  (check (date-year-day d2) => 60)
  (check (date-year-day d3) => 61)
  (check (date-year-day d4) => 365)
  (check (date-year-day d5) => 366)
) ;let

;; Test date-year-day error conditions
(check-catch 'wrong-type-arg (date-year-day "not-a-date"))
(check-catch 'value-error (date-year-day (make-date 0 0 0 0 1 0 2023 0)))
(check-catch 'value-error (date-year-day (make-date 0 0 0 0 1 13 2023 0)))

(check-report)
