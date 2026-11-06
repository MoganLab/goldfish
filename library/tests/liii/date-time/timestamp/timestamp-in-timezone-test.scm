(import (liii check)
        (liii date-time timezone)
        (liii date-time timestamp))

(check-set-mode! 'report-failed)

;; timestamp-in-timezone
;; 将时间戳切换为指定时区表示的同等时刻。
;;
;; 语法
;; ----
;; (timestamp-in-timezone ts tz)
;;
;; 参数
;; ----
;; ts : timestamp?
;; tz : timezone?
;;
;; 返回值
;; ----
;; timestamp?
;;
;; 示例
;; ----
;; (timestamp-in-timezone t2 (tz-timezone "Europe/Vilnius"))

(let ((t1 (make-timestamp 2026 2 3 5 0 0 (tz-timezone "Europe/Vilnius")))
      (t2 (make-timestamp 2026 2 2 22 0 0 (tz-timezone "America/New_York"))))
  (check-true (timestamp=? t1 (timestamp-in-timezone t2 (tz-timezone "Europe/Vilnius"))))
  (check-true (timestamp=? t2 (timestamp-in-timezone t1 (tz-timezone "America/New_York")))))

(check-catch 'type-error (timestamp-in-timezone 123 (utc-timezone)))

(check-report)
