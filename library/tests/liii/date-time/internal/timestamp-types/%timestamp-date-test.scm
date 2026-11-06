;; %timestamp-date
;; 读取 timestamp 记录的日期字段。
;;
;; 语法
;; ----
;; (%timestamp-date ts)
;;
;; 参数
;; ----
;; ts : timestamp? 时间戳对象。
;;
;; 返回值
;; ----
;; date? 时间戳中的日期部分。

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

(check-true (date? (%timestamp-date ts)))
(check (date-year (%timestamp-date ts)) => 2021)

(check-report)
