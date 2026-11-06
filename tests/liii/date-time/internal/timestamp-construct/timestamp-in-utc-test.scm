;; timestamp-in-utc
;; 将时间戳换算为相同物理时刻的 UTC 时间戳。
;;
;; 语法
;; ----
;; (timestamp-in-utc ts)
;;
;; 参数
;; ----
;; ts : timestamp? 时间戳对象。
;;
;; 返回值
;; ----
;; timestamp? 位于 UTC 时区、fold 为 0 的新时间戳。

(import (liii check)
  (liii date-time date)
  (liii date-time clock-time)
  (liii date-time time-delta)
  (liii date-time timezone)
  (liii date-time timestamp)
  (liii date-time internal timestamp-construct)
) ;import

(check-set-mode! 'report-failed)

(define ts (make-timestamp 2021 1 1 12 0 0 (tz-timezone "Asia/Shanghai")))

(define utc (timestamp-in-utc ts))

(check (list (timestamp-hour utc) (timestamp-day utc)) => '(4 1))
(check (timestamp-timezone-offset utc) => (make-time-delta 0 0 0 0 0 0 0))

(check-report)
