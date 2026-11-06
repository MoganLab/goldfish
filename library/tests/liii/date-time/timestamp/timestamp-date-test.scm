(import (liii check)
        (liii date-time date)
        (liii date-time timezone)
        (liii date-time timestamp))

(check-set-mode! 'report-failed)

;; timestamp-date
;; 获取时间戳对象的公历日期部分。
;;
;; 语法
;; ----
;; (timestamp-date ts)
;;
;; 参数
;; ----
;; ts : timestamp?
;;
;; 返回值
;; ----
;; date?
;;
;; 示例
;; ----
;; (timestamp-date (make-timestamp 2020 1 1 10 0 0 (utc-timezone))) => (make-date 2020 1 1)

(check-true (date=? (timestamp-date (make-timestamp 2020 1 1 10 0 0 (utc-timezone)))
                    (make-date 2020 1 1)))

(check-catch 'type-error (timestamp-date "invalid"))

(check-report)
