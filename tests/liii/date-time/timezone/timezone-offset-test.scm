(import (liii check)
        (liii date-time timezone)
        (liii date-time time-delta))

(check-set-mode! 'report-failed)

;; timezone-offset
;; 查询时区在指定 UTC 时刻的偏移。
;;
;; 语法
;; ----
;; (timezone-offset tz)
;; (timezone-offset tz posix-time)
;;
;; 参数
;; ----
;; tz : timezone? 时区对象
;; posix-time : 精确整数，UNIX 纪元起的秒数；省略时取当前时刻
;;
;; 返回值
;; ----
;; time-delta? 该时刻的 UTC 偏移。
;;
;; 示例
;; ----
;; (time-delta=? (timezone-offset (utc-offset-timezone (hours-delta 8)))
;;               (hours-delta 8)) => #t

(define ny (tz-timezone "America/New_York"))

;; 冬季（2025-01-15 00:00 UTC）纽约为 EST，-05:00
(check-true (time-delta=? (timezone-offset ny 1736899200) (hours-delta -5)))
;; 夏季（2025-07-15 00:00 UTC）纽约为 EDT，-04:00
(check-true (time-delta=? (timezone-offset ny 1752537600) (hours-delta -4)))

;; UTC 时区任意时刻偏移为 0
(check-true (time-delta=? (timezone-offset (utc-timezone) 0) (seconds-delta 0)))
(check-true (time-delta=? (timezone-offset (utc-timezone) 1752537600) (seconds-delta 0)))

;; 固定偏移时区
(check-true
  (time-delta=?
    (timezone-offset (utc-offset-timezone (hours-delta 8)) 0)
    (hours-delta 8)))

;; 判断东八区的惯用法
(check-true
  (time-delta=?
    (timezone-offset (utc-offset-timezone (hours-delta 8)))
    (hours-delta 8)))

;; 省略 posix-time 时取当前时刻，返回值类型为 time-delta
(check-true (time-delta? (timezone-offset ny)))
(check-true (time-delta? (timezone-offset (system-timezone))))

;; 非法参数抛出 type-error
(check-catch 'type-error (timezone-offset "Asia/Shanghai"))
(check-catch 'type-error (timezone-offset ny "not an integer"))
(check-catch 'type-error (timezone-offset ny 1736899200.5))

(check-report)
