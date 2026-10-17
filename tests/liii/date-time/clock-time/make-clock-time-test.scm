(import (liii check)
        (liii date-time clock-time))

(check-set-mode! 'report-failed)

;; make-clock-time
;; 创建一天内的时钟时间对象（clock-time）。
;;
;; 语法
;; ----
;; (make-clock-time hour minute second)
;;
;; 参数
;; ----
;; hour : integer? 小时（0-23 的精确整数）。
;; minute : integer? 分钟（0-59 的精确整数）。
;; second : rational? 秒数（0 <= second < 61 的精确有理数或整数，支持小数秒和闰秒60）。
;;
;; 返回值
;; ----
;; clock-time? 新创建的时钟时间对象。
;;
;; 示例
;; ----
;; (make-clock-time 23 59 59)
;; (make-clock-time 12 30 (+ 45 1/2))
;;
;; 错误处理
;; --------
;; type-error 当参数不是精确数字或超出范围时抛出错误。

(check-true (clock-time? (make-clock-time 0 0 0)))
(check-true (clock-time? (make-clock-time 23 59 59)))
(check-true (clock-time? (make-clock-time 23 0 11/10)))
(check-true (clock-time? (make-clock-time 23 59 60)))
(check-true (clock-time? (make-clock-time 23 59 (+ 60 1/2))))

(check-catch 'type-error (make-clock-time 24 0 0))
(check-catch 'type-error (make-clock-time -1 0 0))
(check-catch 'type-error (make-clock-time 23 60 0))
(check-catch 'type-error (make-clock-time 23 -1 0))
(check-catch 'type-error (make-clock-time 23 0 -1))
(check-catch 'type-error (make-clock-time 23 0 61))
(check-catch 'type-error (make-clock-time 23.5 0 0))
(check-catch 'type-error (make-clock-time 23 0 0.5))
(check-catch 'type-error (make-clock-time 'foo 0 0))

(check-report)
