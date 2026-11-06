(import (liii check)
        (liii date-time clock-time)
        (liii date-time timezone)
        (liii date-time timestamp))

(check-set-mode! 'report-failed)

;; timestamp-clock-time
;; 获取时间戳对象的时钟时间部分。
;;
;; 语法
;; ----
;; (timestamp-clock-time ts)
;;
;; 参数
;; ----
;; ts : timestamp?
;;
;; 返回值
;; ----
;; clock-time?
;;
;; 示例
;; ----
;; (timestamp-clock-time (make-timestamp 2020 1 1 10 1 2 (utc-timezone)))

(let ((ct (timestamp-clock-time (make-timestamp 2020 1 1 10 1 2 (utc-timezone)))))
  (check (clock-time-hour ct) => 10)
  (check (clock-time-minute ct) => 1)
  (check (clock-time-second ct) => 2))

(check-catch 'type-error (timestamp-clock-time #f))

(check-report)
