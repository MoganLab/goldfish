;; ts-no-rules
;; 不应用时区 DST 规则地给时间戳加上时间增量。
;;
;; 语法
;; ----
;; (ts-no-rules ts dt)
;;
;; 参数
;; ----
;; ts : timestamp? 时间戳对象。
;; dt : time-delta? 时间增量。
;;
;; 返回值
;; ----
;; timestamp? 相加后的新时间戳，fold 归零。

(import (liii check)
  (liii date-time date)
  (liii date-time clock-time)
  (liii date-time time-delta)
  (liii date-time timezone)
  (liii date-time timestamp)
  (liii date-time internal timestamp-construct)
) ;import

(check-set-mode! 'report-failed)

(define ts (make-timestamp 2021 1 1 0 0 0 (utc-timezone)))

(define r (ts-no-rules ts (hours-delta 25)))

(check (list (timestamp-day r) (timestamp-hour r)) => '(2 1))
(check (timestamp-fold (ts-no-rules ts (seconds-delta 0))) => 0)

(check-report)
