(import (liii check)
        (liii time)
        (srfi srfi-19)
) ;import

(check-set-mode! 'report-failed)

;; date-week-day
;; 获取日期是星期几（周日=0，周一=1，...）。
;;
;; 语法
;; ----
;; (date-week-day date)
;;
;; 参数
;; ----
;; date : date? 日期对象。
;;
;; 返回值
;; ----
;; integer? 星期几的编号，范围 0-6。
;;
;; 错误处理
;; --------
;; wrong-type-arg 当参数不是日期对象时抛出错误。

;; Test date-week-day
(let ((d1 (make-date 0 0 0 0 1 1 1970 0))   ; 1970-01-01 Thu
      (d2 (make-date 0 0 0 0 25 12 2023 0)) ; 2023-12-25 Mon
      (d3 (make-date 0 0 0 0 29 2 2024 0))) ; 2024-02-29 Thu
  (check (date-week-day d1) => 4)
  (check (date-week-day d2) => 1)
  (check (date-week-day d3) => 4)
) ;let

;; Test date-week-day error conditions
(check-catch 'wrong-type-arg (date-week-day "not-a-date"))

(check-report)
