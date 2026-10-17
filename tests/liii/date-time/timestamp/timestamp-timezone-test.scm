(import (liii check)
        (liii date-time timezone)
        (liii date-time timestamp))

(check-set-mode! 'report-failed)

;; timestamp-timezone
;; 获取时间戳对象的时区对象。
;;
;; 语法
;; ----
;; (timestamp-timezone ts)
;;
;; 参数
;; ----
;; ts : timestamp?
;;
;; 返回值
;; ----
;; timezone?
;;
;; 示例
;; ----
;; (timestamp-timezone (make-timestamp 2020 1 1 10 0 0 (utc-timezone)))

(let ((tz (utc-timezone)))
  (check-true (timezone? (timestamp-timezone (make-timestamp 2020 1 1 10 0 0 tz)))))

(check-catch 'type-error (timestamp-timezone 123))

(check-report)
