;; %timestamp-timezone
;; 读取 timestamp 记录的时区字段。
;;
;; 语法
;; ----
;; (%timestamp-timezone ts)
;;
;; 参数
;; ----
;; ts : timestamp? 时间戳对象。
;;
;; 返回值
;; ----
;; timezone? 时间戳绑定的时区对象。

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

(check-true (timezone? (%timestamp-timezone ts)))

(check-report)
