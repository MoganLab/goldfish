(import (liii check)
        (liii date-time date))

(check-set-mode! 'report-failed)

;; date<?
;; 比较日期对象是否严格单调递增（小于）。
;;
;; 语法
;; ----
;; (date<? date1 date2 ...)
;;
;; 参数
;; ----
;; date1, date2, ... : date? 日期对象。
;;
;; 返回值
;; ----
;; boolean? 若严格单调递增返回 #t，否则返回 #f。
;;
;; 示例
;; ----
;; (date<? (make-date 2020 1 1) (make-date 2020 1 2)) => #t
;;
;; 错误处理
;; --------
;; type-error 当任一参数不是日期对象时抛出错误。

(check-true (date<? (make-date 2020 2 2) (make-date 2021 1 1)))
(check-true (date<? (make-date 2020 2 2) (make-date 2020 3 1)))
(check-true (date<? (make-date 2020 2 2) (make-date 2020 2 3)))
(check-true (date<? (make-date 2020 1 1) (make-date 2020 1 2) (make-date 2020 1 3)))
(check-false (date<? (make-date 2020 1 1) (make-date 2020 1 3) (make-date 2020 1 2)))
(check-false (date<? (make-date 2020 2 2) (make-date 2020 2 2)))
(check-false (date<? (make-date 2021 1 1) (make-date 2020 2 2)))
(check-false (date<? (make-date 2020 3 1) (make-date 2020 2 2)))
(check-false (date<? (make-date 2020 2 3) (make-date 2020 2 2)))

(check-catch 'type-error (date<? (make-date 2020 1 1) 2020))
(check-catch 'type-error (date<? #f (make-date 2020 1 1)))

(check-report)
