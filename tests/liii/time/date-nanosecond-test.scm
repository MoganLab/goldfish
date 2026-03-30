(import (liii check)
        (liii time)
        (srfi srfi-19)
) ;import

(check-set-mode! 'report-failed)

;; date-nanosecond
;; 获取日期对象的纳秒部分。
;;
;; 语法
;; ----
;; (date-nanosecond date)
;;
;; 参数
;; ----
;; date : date? 日期对象。
;;
;; 返回值
;; ----
;; integer? 纳秒部分。
;;
;; 示例
;; ----
;; (date-nanosecond (make-date 123456789 0 0 0 1 1 2023 0)) => 123456789
;;
;; 错误处理
;; --------
;; wrong-type-arg 当参数不是日期对象时抛出错误。

(let ((d1 (make-date 123456789 45 30 14 25 12 2023 28800))
      (d2 (make-date 999999999 59 59 23 31 1 2000 -14400)))
  (check (date-nanosecond d1) => 123456789)
  (check (date-nanosecond d2) => 999999999)
) ;let

(check-catch 'wrong-type-arg (date-nanosecond "not-a-date"))

(check-report)
