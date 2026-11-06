(import (liii check)
        (liii date-time timezone)
        (liii date-time timestamp))

(check-set-mode! 'report-failed)

;; timestamp=?
;; 比较两个或多个时间戳在物理绝对时间上是否等价。
;;
;; 语法
;; ----
;; (timestamp=? ts1 ts2 ...)
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
;; (timestamp=? (make-timestamp 2026 2 3 5 0 0 (tz-timezone "Europe/Vilnius"))
;;              (make-timestamp 2026 2 2 22 0 0 (tz-timezone "America/New_York"))) => #t

(let ((t1 (make-timestamp 2026 2 3 5 0 0 (tz-timezone "Europe/Vilnius")))
      (t2 (make-timestamp 2026 2 2 22 0 0 (tz-timezone "America/New_York"))))
  (check-true (timestamp=? t1 t2))
  (check-true (timestamp=? t1 t1 t2))
  (check-false (timestamp=? t1 (make-timestamp 2026 2 3 6 0 0 (tz-timezone "Europe/Vilnius")))))

(check-catch 'type-error (timestamp=? (make-timestamp 2020 1 1 0 0 0 (utc-timezone)) "invalid"))

(check-report)
