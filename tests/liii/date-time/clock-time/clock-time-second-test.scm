(import (liii check)
        (liii date-time clock-time))

(check-set-mode! 'report-failed)

;; clock-time-second
;; 获取时钟时间对象的秒数（精确有理数或整数）。
;;
;; 语法
;; ----
;; (clock-time-second ct)
;;
;; 参数
;; ----
;; ct : clock-time? 时钟时间对象。
;;
;; 返回值
;; ----
;; rational? 秒数（0 <= second < 61）。
;;
;; 示例
;; ----
;; (clock-time-second (make-clock-time 23 1 2)) => 2
;; (clock-time-second (make-clock-time 23 1 11/10)) => 11/10
;;
;; 错误处理
;; --------
;; type-error 当参数不是 clock-time 对象时抛出错误。

(check (clock-time-second (make-clock-time 23 1 2)) => 2)
(check (clock-time-second (make-clock-time 23 1 11/10)) => 11/10)
(check (clock-time-second (make-clock-time 23 1 (+ 60 1/2))) => (+ 60 1/2))

(check-catch 'type-error (clock-time-second "2"))
(check-catch 'type-error (clock-time-second #f))

(check-report)
