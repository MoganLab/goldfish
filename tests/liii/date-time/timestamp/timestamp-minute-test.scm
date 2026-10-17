(import (liii check)
        (liii date-time timezone)
        (liii date-time timestamp))

(check-set-mode! 'report-failed)

;; timestamp-minute
;; 获取时间戳对象的分钟数。
;;
;; 语法
;; ----
;; (timestamp-minute ts)
;;
;; 参数
;; ----
;; ts : timestamp?
;;
;; 返回值
;; ----
;; integer? 分钟。
;;
;; 示例
;; ----
;; (timestamp-minute (make-timestamp 2020 1 1 10 1 2 (utc-timezone))) => 1

(check (timestamp-minute (make-timestamp 2020 1 1 10 1 2 (utc-timezone))) => 1)
(check-catch 'type-error (timestamp-minute "1"))

(check-report)
