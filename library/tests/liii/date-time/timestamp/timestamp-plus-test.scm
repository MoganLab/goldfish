(import (liii check)
        (liii date-time time-delta)
        (liii date-time timezone)
        (liii date-time timestamp))

(check-set-mode! 'report-failed)

;; timestamp+
;; 将时间戳与时间增量相加，正确处理夏令时跃变、折叠与闰秒。
;;
;; 语法
;; ----
;; (timestamp+ ts dt)
;;
;; 参数
;; ----
;; ts : timestamp?
;; dt : time-delta?
;;
;; 返回值
;; ----
;; timestamp?
;;
;; 示例
;; ----
;; (timestamp+ (make-timestamp 2021 1 1 0 0 0 (utc-timezone)) (hours-delta 1))

;; 基础加法
(check-true (timestamp=? (make-timestamp 2021 1 1 1 0 0 (utc-timezone))
                         (timestamp+ (make-timestamp 2021 1 1 0 0 0 (utc-timezone)) (hours-delta 1))))

(check-true (timestamp=? (make-timestamp 2020 12 31 23 0 0 (utc-timezone))
                         (timestamp+ (make-timestamp 2021 1 1 0 0 0 (utc-timezone)) (hours-delta -1))))

;; 夏令时进入时跳过 1 小时
(check-true (timestamp=? (make-timestamp 2025 3 30 4 30 0 (tz-timezone "Europe/Vilnius"))
                         (timestamp+ (make-timestamp 2025 3 30 2 30 0 (tz-timezone "Europe/Vilnius")) (hours-delta 1))))

;; 夏令时退出时产生相同本地时间并设置 fold 标志
(check-true (timestamp=? (make-timestamp 2025 10 26 3 30 0 (tz-timezone "Europe/Vilnius") 1)
                         (timestamp+ (make-timestamp 2025 10 26 3 30 0 (tz-timezone "Europe/Vilnius")) (hours-delta 1))))

(check-catch 'type-error (timestamp+ 100 (hours-delta 1)))
(check-catch 'type-error (timestamp+ (make-timestamp 2021 1 1 0 0 0 (utc-timezone)) 10))

(check-report)
