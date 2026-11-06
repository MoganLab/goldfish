(import (liii check)
        (liii date-time date)
        (liii date-time moment))

(check-set-mode! 'report-failed)

;; moment-second-of-day
;; 获取时刻对象在当天经过的秒数。
;;
;; 语法
;; ----
;; (moment-second-of-day m)
;;
;; 参数
;; ----
;; m : moment? 时刻对象。
;;
;; 返回值
;; ----
;; real? 秒数（精确数字）。
;;
;; 示例
;; ----
;; (moment-second-of-day (make-moment (make-date 2025 1 1) 120)) => 120
;;
;; 错误处理
;; --------
;; type-error 当参数不是 moment 对象时抛出错误。

(check (moment-second-of-day (make-moment (make-date 2025 1 1) 1)) => 1)
(check (moment-second-of-day (make-moment (make-date 2025 1 1) (+ 3600 1/3))) => (+ 3600 1/3))

(check-catch 'type-error (moment-second-of-day (make-date 2025 1 1)))
(check-catch 'type-error (moment-second-of-day #f))

(check-report)
