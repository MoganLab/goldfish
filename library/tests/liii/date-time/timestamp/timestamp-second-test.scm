(import (liii check)
        (liii date-time timezone)
        (liii date-time timestamp))

(check-set-mode! 'report-failed)

;; timestamp-second
;; 获取时间戳对象的秒数（精确有理数或整数）。
;;
;; 语法
;; ----
;; (timestamp-second ts)
;;
;; 参数
;; ----
;; ts : timestamp?
;;
;; 返回值
;; ----
;; rational? 秒。
;;
;; 示例
;; ----
;; (timestamp-second (make-timestamp 2020 1 1 10 1 2 (utc-timezone))) => 2

(check (timestamp-second (make-timestamp 2020 1 1 10 1 2 (utc-timezone))) => 2)
(check-catch 'type-error (timestamp-second #f))

(check-report)
