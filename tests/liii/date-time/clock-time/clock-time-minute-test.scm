(import (liii check)
        (liii date-time clock-time))

(check-set-mode! 'report-failed)

;; clock-time-minute
;; 获取时钟时间对象的分钟数。
;;
;; 语法
;; ----
;; (clock-time-minute ct)
;;
;; 参数
;; ----
;; ct : clock-time? 时钟时间对象。
;;
;; 返回值
;; ----
;; integer? 分钟（0-59）。
;;
;; 示例
;; ----
;; (clock-time-minute (make-clock-time 23 1 2)) => 1
;;
;; 错误处理
;; --------
;; type-error 当参数不是 clock-time 对象时抛出错误。

(check (clock-time-minute (make-clock-time 23 1 2)) => 1)
(check (clock-time-minute (make-clock-time 10 59 0)) => 59)

(check-catch 'type-error (clock-time-minute "invalid"))
(check-catch 'type-error (clock-time-minute 'foo))

(check-report)
