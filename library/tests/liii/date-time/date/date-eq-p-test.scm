(import (liii check)
        (liii date-time date))

(check-set-mode! 'report-failed)

;; date=?
;; 比较两个或多个日期对象是否相等。
;;
;; 语法
;; ----
;; (date=? date1 date2 ...)
;;
;; 参数
;; ----
;; date1, date2, ... : date? 日期对象。
;;
;; 返回值
;; ----
;; boolean? 若所有日期均相等返回 #t，否则返回 #f。
;;
;; 示例
;; ----
;; (date=? (make-date 2020 1 1) (make-date 2020 1 1)) => #t
;; (date=? (make-date 2020 1 1) (make-date 2021 1 1)) => #f
;;
;; 错误处理
;; --------
;; type-error 当任一参数不是日期对象时抛出错误。

(check-true (date=? (make-date 2020 1 1) (make-date 2020 1 1)))
(check-true (date=? (make-date 2020 1 1) (make-date 2020 1 1) (make-date 2020 1 1)))
(check-false (date=? (make-date 2021 1 1) (make-date 2020 1 1)))
(check-false (date=? (make-date 2020 2 1) (make-date 2020 1 1)))
(check-false (date=? (make-date 2020 1 2) (make-date 2020 1 1)))

(check-catch 'type-error (date=? (make-date 2020 1 1) "2020-01-01"))
(check-catch 'type-error (date=? 'foo (make-date 2020 1 1)))

(check-report)
