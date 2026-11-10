;; timestamp<=?
;; 比较时间戳是否单调递增（小于或等于）。
;;
;; 语法
;; ----
;; (timestamp<=? ts1 ts2 ...)
;;
;; 参数
;; ----
;; ts1, ts2 ... : timestamp?
;;
;; 返回值
;; ----
;; boolean?

(import (liii check) (liii date-time timezone) (liii date-time timestamp))

(check-set-mode! 'report-failed)


(check-true (timestamp<=? (make-timestamp 2026 1 1 0 0 0 (utc-timezone))
              (make-timestamp 2026 1 1 0 0 0 (utc-timezone))
            ) ;timestamp<=?
) ;check-true
(check-true (timestamp<=? (make-timestamp 2026 1 1 0 0 0 (utc-timezone))
              (make-timestamp 2026 1 1 1 0 0 (utc-timezone))
            ) ;timestamp<=?
) ;check-true
(check-false (timestamp<=? (make-timestamp 2026 1 1 1 0 0 (utc-timezone))
               (make-timestamp 2026 1 1 0 0 0 (utc-timezone))
             ) ;timestamp<=?
) ;check-false

(check-catch 'type-error
  (timestamp<=? (make-timestamp 2026 1 1 0 0 0 (utc-timezone)) #f)
) ;check-catch

(check-report)
