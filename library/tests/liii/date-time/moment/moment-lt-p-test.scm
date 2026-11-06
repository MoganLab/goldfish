(import (liii check)
        (liii date-time date)
        (liii date-time moment))

(check-set-mode! 'report-failed)

;; moment<?
;; 比较时刻对象是否严格单调递增（小于）。
;;
;; 语法
;; ----
;; (moment<? m1 m2 ...)
;;
;; 参数
;; ----
;; m1, m2, ... : moment? 时刻对象。
;;
;; 返回值
;; ----
;; boolean? 若严格单调递增返回 #t，否则返回 #f。
;;
;; 示例
;; ----
;; (moment<? (make-moment (make-date 2024 1 1) 2) (make-moment (make-date 2025 1 1) 1)) => #t
;;
;; 错误处理
;; --------
;; type-error 当任一参数不是 moment 对象时抛出错误。

(check-true (moment<? (make-moment (make-date 2024 1 1) 2)
                      (make-moment (make-date 2025 1 1) 1)))
(check-true (moment<? (make-moment (make-date 2025 1 1) 1)
                      (make-moment (make-date 2025 1 1) 2)))
(check-true (moment<? (make-moment (make-date 2025 1 1) 1)
                      (make-moment (make-date 2025 1 1) 2)
                      (make-moment (make-date 2025 1 2) 0)))
(check-false (moment<? (make-moment (make-date 2025 1 1) 1)
                       (make-moment (make-date 2025 1 1) 1)))
(check-false (moment<? (make-moment (make-date 2026 1 1) 1)
                       (make-moment (make-date 2025 1 1) 1)))
(check-false (moment<? (make-moment (make-date 2025 1 1) 2)
                       (make-moment (make-date 2025 1 1) 1)))

(check-catch 'type-error (moment<? (make-moment (make-date 2025 1 1) 1) 100))

(check-report)
