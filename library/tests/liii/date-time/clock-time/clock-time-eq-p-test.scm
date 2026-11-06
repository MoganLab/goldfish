(import (liii check)
        (liii date-time clock-time))

(check-set-mode! 'report-failed)

;; clock-time=?
;; 比较两个或多个 clock-time 是否相等。
;;
;; 语法
;; ----
;; (clock-time=? ct1 ct2 ...)
;;
;; 参数
;; ----
;; ct1, ct2, ... : clock-time? 时钟时间对象。
;;
;; 返回值
;; ----
;; boolean? 若均相等返回 #t，否则返回 #f。
;;
;; 示例
;; ----
;; (clock-time=? (make-clock-time 12 0 0) (make-clock-time 12 0 0)) => #t
;;
;; 错误处理
;; --------
;; type-error 当任一参数不是 clock-time 对象时抛出错误。

(check-true (clock-time=? (make-clock-time 12 0 0) (make-clock-time 12 0 0)))
(check-true (clock-time=? (make-clock-time 12 0 0) (make-clock-time 12 0 0) (make-clock-time 12 0 0)))
(check-false (clock-time=? (make-clock-time 12 0 0) (make-clock-time 12 0 1)))
(check-false (clock-time=? (make-clock-time 12 0 0) (make-clock-time 12 1 0)))
(check-false (clock-time=? (make-clock-time 12 0 0) (make-clock-time 13 0 0)))

(check-catch 'type-error (clock-time=? (make-clock-time 12 0 0) "12:00:00"))
(check-catch 'type-error (clock-time=? 'foo (make-clock-time 12 0 0)))

(check-report)
