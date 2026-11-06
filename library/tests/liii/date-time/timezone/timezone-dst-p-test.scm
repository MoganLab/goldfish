(import (liii check)
        (liii date-time timezone)
        (liii date-time time-delta))

(check-set-mode! 'report-failed)

;; timezone-dst?
;; 查询时区在指定 UTC 时刻是否处于夏令时（DST）。
;;
;; 语法
;; ----
;; (timezone-dst? tz)
;; (timezone-dst? tz posix-time)
;;
;; 参数
;; ----
;; tz : timezone? 时区对象
;; posix-time : 精确整数，UNIX 纪元起的秒数；省略时取当前时刻
;;
;; 返回值
;; ----
;; boolean? 该时刻处于夏令时为 #t。
;;
;; 示例
;; ----
;; (timezone-dst? (tz-timezone "America/New_York") 1752537600) => #t

(define ny (tz-timezone "America/New_York"))

;; 冬季（2025-01-15）纽约为 EST，非夏令时
(check-false (timezone-dst? ny 1736899200))
;; 夏季（2025-07-15）纽约为 EDT，处于夏令时
(check-true (timezone-dst? ny 1752537600))

;; UTC 时区不实行夏令时
(check-false (timezone-dst? (utc-timezone) 1752537600))
;; 固定偏移时区不实行夏令时
(check-false (timezone-dst? (utc-offset-timezone (hours-delta 8)) 1752537600))

;; 省略 posix-time 时取当前时刻，返回值类型为 boolean
(check-true (boolean? (timezone-dst? ny)))
(check-true (boolean? (timezone-dst? (system-timezone))))

;; 非法参数抛出 type-error
(check-catch 'type-error (timezone-dst? "Asia/Shanghai"))
(check-catch 'type-error (timezone-dst? ny "not an integer"))

(check-report)
