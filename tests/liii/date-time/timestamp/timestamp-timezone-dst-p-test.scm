(import (liii check)
        (liii date-time timezone)
        (liii date-time timestamp))

(check-set-mode! 'report-failed)

;; timestamp-timezone-dst?
;; 判断时间戳当前是否处于夏令时（DST）。
;;
;; 语法
;; ----
;; (timestamp-timezone-dst? ts)
;;
;; 参数
;; ----
;; ts : timestamp?
;;
;; 返回值
;; ----
;; boolean?
;;
;; 示例
;; ----
;; (timestamp-timezone-dst? ts)

(let ((dst (make-timestamp 2026 3 8 12 0 0 (tz-timezone "America/New_York")))
      (std (make-timestamp 2026 3 8 1 0 0 (tz-timezone "America/New_York"))))
  (check-true (timestamp-timezone-dst? dst))
  (check-false (timestamp-timezone-dst? std)))

(check-catch 'type-error (timestamp-timezone-dst? #f))

(check-report)
