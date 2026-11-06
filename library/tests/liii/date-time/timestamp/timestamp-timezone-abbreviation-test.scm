(import (liii check)
        (liii date-time timezone)
        (liii date-time timestamp))

(check-set-mode! 'report-failed)

;; timestamp-timezone-abbreviation
;; 获取时间戳在当前时区下的缩写名称（如 "EDT", "EST", "UTC"）。
;;
;; 语法
;; ----
;; (timestamp-timezone-abbreviation ts)
;;
;; 参数
;; ----
;; ts : timestamp?
;;
;; 返回值
;; ----
;; string?
;;
;; 示例
;; ----
;; (timestamp-timezone-abbreviation ts)

(let ((dst (make-timestamp 2026 3 8 12 0 0 (tz-timezone "America/New_York")))
      (std (make-timestamp 2026 3 8 1 0 0 (tz-timezone "America/New_York"))))
  (check (timestamp-timezone-abbreviation dst) => "EDT")
  (check (timestamp-timezone-abbreviation std) => "EST"))

(check-catch 'type-error (timestamp-timezone-abbreviation 'foo))

(check-report)
