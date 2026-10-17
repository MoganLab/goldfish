(import (liii check)
        (liii date-time timezone)
        (liii date-time timestamp))

(check-set-mode! 'report-failed)

;; timestamp-month
;; 获取时间戳对象的月份。
;;
;; 语法
;; ----
;; (timestamp-month ts)
;;
;; 参数
;; ----
;; ts : timestamp?
;;
;; 返回值
;; ----
;; integer? 月份（1-12）。
;;
;; 示例
;; ----
;; (timestamp-month (make-timestamp 2020 1 2 10 0 0 (utc-timezone))) => 1

(check (timestamp-month (make-timestamp 2020 1 2 10 0 0 (utc-timezone))) => 1)
(check-catch 'type-error (timestamp-month #f))

(check-report)
