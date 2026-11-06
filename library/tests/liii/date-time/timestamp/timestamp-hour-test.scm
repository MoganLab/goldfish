(import (liii check)
        (liii date-time timezone)
        (liii date-time timestamp))

(check-set-mode! 'report-failed)

;; timestamp-hour
;; 获取时间戳对象的小时数。
;;
;; 语法
;; ----
;; (timestamp-hour ts)
;;
;; 参数
;; ----
;; ts : timestamp?
;;
;; 返回值
;; ----
;; integer? 小时。
;;
;; 示例
;; ----
;; (timestamp-hour (make-timestamp 2020 1 1 10 1 2 (utc-timezone))) => 10

(check (timestamp-hour (make-timestamp 2020 1 1 10 1 2 (utc-timezone))) => 10)
(check-catch 'type-error (timestamp-hour 10))

(check-report)
