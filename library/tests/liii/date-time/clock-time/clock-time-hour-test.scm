(import (liii check)
        (liii date-time clock-time))

(check-set-mode! 'report-failed)

;; clock-time-hour
;; 获取时钟时间对象的小时数。
;;
;; 语法
;; ----
;; (clock-time-hour ct)
;;
;; 参数
;; ----
;; ct : clock-time? 时钟时间对象。
;;
;; 返回值
;; ----
;; integer? 小时（0-23）。
;;
;; 示例
;; ----
;; (clock-time-hour (make-clock-time 23 1 2)) => 23
;;
;; 错误处理
;; --------
;; type-error 当参数不是 clock-time 对象时抛出错误。

(check (clock-time-hour (make-clock-time 23 1 2)) => 23)
(check (clock-time-hour (make-clock-time 0 59 59)) => 0)

(check-catch 'type-error (clock-time-hour "23:01:02"))
(check-catch 'type-error (clock-time-hour 23))

(check-report)
