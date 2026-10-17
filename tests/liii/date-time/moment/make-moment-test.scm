(import (liii check)
        (liii date-time date)
        (liii date-time moment))

(check-set-mode! 'report-failed)

;; make-moment
;; 创建一个时刻对象（公历日期 + 从当天 00:00:00 起经过的秒数）。
;;
;; 语法
;; ----
;; (make-moment date second-of-day)
;;
;; 参数
;; ----
;; date : date? 日期对象。
;; second-of-day : real? 当天经过的秒数（0 <= second < 86400 的精确数字）。
;;
;; 返回值
;; ----
;; moment? 时刻对象。
;;
;; 示例
;; ----
;; (make-moment (make-date 2025 1 1) 100)
;;
;; 错误处理
;; --------
;; type-error 当参数不是合法日期或秒数超出范围时抛出错误。

(check-true (moment? (make-moment (make-date 2025 1 1) 0)))
(check-true (moment? (make-moment (make-date 2025 1 1) 100)))
(check-true (moment? (make-moment (make-date 2025 1 1) 86399)))
(check-true (moment? (make-moment (make-date 2025 1 1) (+ 86399 1/2))))

(check-catch 'type-error (make-moment #f 100))
(check-catch 'type-error (make-moment (make-date 2025 1 1) -1))
(check-catch 'type-error (make-moment (make-date 2025 1 1) 86400))
(check-catch 'type-error (make-moment (make-date 2025 1 1) 86401))
(check-catch 'type-error (make-moment (make-date 2025 1 1) 100.5))

(check-report)
