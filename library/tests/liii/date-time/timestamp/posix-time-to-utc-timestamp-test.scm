(import (liii check)
        (liii date-time timezone)
        (liii date-time timestamp))

(check-set-mode! 'report-failed)

;; posix-time->utc-timestamp
;; 将 POSIX 时间（从 1970-01-01T00:00:00Z 起算的秒数）转换为 UTC 时间戳。
;;
;; 语法
;; ----
;; (posix-time->utc-timestamp seconds)
;; (posix-time->utc-timestamp seconds nanoseconds)
;;
;; 参数
;; ----
;; seconds : rational?
;; nanoseconds : integer? 可选。
;;
;; 返回值
;; ----
;; timestamp?
;;
;; 示例
;; ----
;; (posix-time->utc-timestamp 0) => (make-timestamp 1970 1 1 0 0 0 (utc-timezone))

(let ((ts (posix-time->utc-timestamp 0)))
  (check (timestamp-year ts) => 1970)
  (check (timestamp-month ts) => 1)
  (check (timestamp-day ts) => 1)
  (check (timestamp-hour ts) => 0)
  (check (timestamp-minute ts) => 0)
  (check (timestamp-second ts) => 0))

(check (timestamp-second (posix-time->utc-timestamp 100 500000000)) => (+ 40 1/2))

(check-catch 'type-error (posix-time->utc-timestamp "0"))

(check-report)
