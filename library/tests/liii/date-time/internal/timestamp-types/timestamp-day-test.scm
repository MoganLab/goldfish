;; timestamp-day
;; 获取时间戳对象的日。
;;
;; 语法
;; ----
;; (timestamp-day ts)
;;
;; 参数
;; ----
;; ts : timestamp?
;;
;; 返回值
;; ----
;; integer? 日。

(import (liii check) (liii date-time timezone) (liii date-time timestamp))

(check-set-mode! 'report-failed)


(check (timestamp-day (make-timestamp 2020 1 2 10 0 0 (utc-timezone))) => 2)
(check-catch 'type-error (timestamp-day "2"))

(check-report)
