(import (liii check)
        (liii time)
        (srfi srfi-19)
) ;import

(check-set-mode! 'report-failed)

;; date-week-number
;; 获取日期在当年的周序号（忽略年初的残周）。
;;
;; 语法
;; ----
;; (date-week-number date day-of-week-starting-week)
;;
;; 参数
;; ----
;; date : date? 日期对象。
;; day-of-week-starting-week : integer? 一周从哪一天开始（周日=0，周一=1，...）。
;;
;; 返回值
;; ----
;; integer? 周序号（从 0 开始计数）。
;;
;; 错误处理
;; --------
;; wrong-type-arg 当参数不是日期对象时抛出错误。

;; Test date-week-number (ignore first partial week)
(let ((d1 (make-date 0 0 0 0 4 1 1970 0))   ; 1970-01-04 Sun
      (d2 (make-date 0 0 0 0 11 1 1970 0))  ; 1970-01-11 Sun
      (d3 (make-date 0 0 0 0 5 1 1970 0))   ; 1970-01-05 Mon
      (d4 (make-date 0 0 0 0 12 1 1970 0))  ; 1970-01-12 Mon
      (d5 (make-date 0 0 0 0 31 12 2024 0)))
  (check (date-week-number d1 0) => 0)
  (check (date-week-number d2 0) => 1)
  (check (date-week-number d3 1) => 0)
  (check (date-week-number d4 1) => 1)
  (check (date-week-number d5 1) => 52)
) ;let

;; Test date-week-number error conditions
(check-catch 'wrong-type-arg (date-week-number "not-a-date" 0))

(check-report)
