(import (liii check)
        (liii time)
        (srfi srfi-19)
) ;import

(check-set-mode! 'report-failed)

;; date-day
;; 获取日期对象的日部分。
;;
;; 语法
;; ----
;; (date-day date)
;;
;; 参数
;; ----
;; date : date? 日期对象。
;;
;; 返回值
;; ----
;; integer? 日部分（1-31）。
;;
;; 示例
;; ----
;; (date-day (make-date 0 0 0 0 25 12 2023 0)) => 25
;;
;; 错误处理
;; --------
;; wrong-type-arg 当参数不是日期对象时抛出错误。

(let ((d1 (make-date 123456789 45 30 14 25 12 2023 28800))
      (d2 (make-date 999999999 59 59 23 31 1 2000 -14400)))
  (check (date-day d1) => 25)
  (check (date-day d2) => 31)
) ;let

(check-catch 'wrong-type-arg (date-day #(vector)))

(check-report)
