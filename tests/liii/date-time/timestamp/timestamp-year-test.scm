(import (liii check)
        (liii date-time timezone)
        (liii date-time timestamp))

(check-set-mode! 'report-failed)

;; timestamp-year
;; 获取时间戳对象的年份。
;;
;; 语法
;; ----
;; (timestamp-year ts)
;;
;; 参数
;; ----
;; ts : timestamp?
;;
;; 返回值
;; ----
;; integer? 年份。
;;
;; 示例
;; ----
;; (timestamp-year (make-timestamp 2020 1 2 10 0 0 (utc-timezone))) => 2020

(check (timestamp-year (make-timestamp 2020 1 2 10 0 0 (utc-timezone))) => 2020)
(check-catch 'type-error (timestamp-year 2020))

(check-report)
