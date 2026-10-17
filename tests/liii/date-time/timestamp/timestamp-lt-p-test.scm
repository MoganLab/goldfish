(import (liii check)
        (liii date-time timezone)
        (liii date-time timestamp))

(check-set-mode! 'report-failed)

;; timestamp<?
;; 比较时间戳是否严格单调递增（小于）。
;;
;; 语法
;; ----
;; (timestamp<? ts1 ts2 ...)
;;
;; 参数
;; ----
;; ts1, ts2 ... : timestamp?
;;
;; 返回值
;; ----
;; boolean?
;;
;; 示例
;; ----
;; (timestamp<? (make-timestamp 2026 1 1 0 0 0 (utc-timezone))
;;              (make-timestamp 2026 1 1 1 0 0 (utc-timezone))) => #t

(check-true (timestamp<? (make-timestamp 2026 1 1 0 0 0 (utc-timezone))
                         (make-timestamp 2026 1 1 1 0 0 (utc-timezone))))
(check-false (timestamp<? (make-timestamp 2026 1 1 0 0 0 (utc-timezone))
                          (make-timestamp 2026 1 1 0 0 0 (utc-timezone))))

(check-catch 'type-error (timestamp<? 123 (make-timestamp 2026 1 1 0 0 0 (utc-timezone))))

(check-report)
