(import (liii check)
        (liii date-time clock-time))

(check-set-mode! 'report-failed)

;; clock-time>=?
;; 比较 clock-time 是否单调递减（大于或等于）。
;;
;; 语法
;; ----
;; (clock-time>=? ct1 ct2 ...)
;;
;; 参数
;; ----
;; ct1, ct2, ... : clock-time? 时钟时间对象。
;;
;; 返回值
;; ----
;; boolean? 若单调递减返回 #t，否则返回 #f。
;;
;; 示例
;; ----
;; (clock-time>=? (make-clock-time 12 0 0) (make-clock-time 12 0 0)) => #t
;;
;; 错误处理
;; --------
;; type-error 当任一参数不是 clock-time 对象时抛出错误。

(check-true (clock-time>=? (make-clock-time 12 0 0) (make-clock-time 11 0 0)))
(check-true (clock-time>=? (make-clock-time 12 0 0) (make-clock-time 12 0 0)))
(check-true (clock-time>=? (make-clock-time 12 0 0) (make-clock-time 12 0 0) (make-clock-time 10 0 0)))
(check-false (clock-time>=? (make-clock-time 11 0 0) (make-clock-time 12 0 0)))

(check-catch 'type-error (clock-time>=? (make-clock-time 12 0 0) "12:00:00"))

(check-report)
