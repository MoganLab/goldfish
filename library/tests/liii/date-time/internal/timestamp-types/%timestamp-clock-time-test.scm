;; %timestamp-clock-time
;; 读取 timestamp 记录的时钟时间字段。
;;
;; 语法
;; ----
;; (%timestamp-clock-time ts)
;;
;; 参数
;; ----
;; ts : timestamp? 时间戳对象。
;;
;; 返回值
;; ----
;; clock-time? 时间戳中的时钟时间部分。

(import (liii check)
  (liii date-time date)
  (liii date-time clock-time)
  (liii date-time timezone)
  (liii date-time internal timestamp-types)
) ;import

(check-set-mode! 'report-failed)

(define ts
  (%make-timestamp (make-date 2021 1 2) (make-clock-time 3 4 5) (utc-timezone) 0)
) ;define

(check-true (clock-time? (%timestamp-clock-time ts)))
(check (clock-time-minute (%timestamp-clock-time ts)) => 4)

(check-report)
