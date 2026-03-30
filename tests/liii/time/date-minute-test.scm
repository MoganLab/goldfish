(import (liii check)
        (liii time)
        (srfi srfi-19)
) ;import

(check-set-mode! 'report-failed)

;; date-minute
;; 获取日期对象的分钟部分。
;;
;; 语法
;; ----
;; (date-minute date)
;;
;; 参数
;; ----
;; date : date? 日期对象。
;;
;; 返回值
;; ----
;; integer? 分钟部分（0-59）。
;;
;; 示例
;; ----
;; (date-minute (make-date 0 0 30 0 1 1 2023 0)) => 30
;;
;; 错误处理
;; --------
;; wrong-type-arg 当参数不是日期对象时抛出错误。

(let ((d1 (make-date 123456789 45 30 14 25 12 2023 28800))
      (d2 (make-date 999999999 59 59 23 31 1 2000 -14400)))
  (check (date-minute d1) => 30)
  (check (date-minute d2) => 59)
) ;let

(check-catch 'wrong-type-arg (date-minute 'symbol))

(check-report)
