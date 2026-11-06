(import (liii check)
        (liii date-time time-delta))

(check-set-mode! 'report-failed)

;; time-delta+
;; 将多个 time-delta 对象的各个时间分量对应相加。
;;
;; 语法
;; ----
;; (time-delta+ dt ...)
;;
;; 参数
;; ----
;; dt ... : time-delta?
;;
;; 返回值
;; ----
;; time-delta? 相加后的新 time-delta。
;;
;; 示例
;; ----
;; (time-delta+ (years-delta 1) (days-delta 5))
;;
;; 错误处理
;; --------
;; type-error 当任一参数不是 time-delta 对象时抛出错误。

(let ((dt (time-delta+ (years-delta 1)
                       (years-delta 10)
                       (months-delta 2)
                       (months-delta 10)
                       (weeks-delta 3)
                       (weeks-delta 10)
                       (days-delta 4)
                       (days-delta 10)
                       (hours-delta 5)
                       (hours-delta 10)
                       (minutes-delta 6)
                       (minutes-delta 10)
                       (seconds-delta 7)
                       (seconds-delta 10))))
  (check-true (time-delta? dt))
  (check (time-delta-years dt) => 11)
  (check (time-delta-months dt) => 12)
  (check (time-delta-weeks dt) => 13)
  (check (time-delta-days dt) => 14)
  (check (time-delta-hours dt) => 15)
  (check (time-delta-minutes dt) => 16)
  (check (time-delta-seconds dt) => 17))

(check-catch 'type-error (time-delta+ (years-delta 1) 1))

(check-report)
