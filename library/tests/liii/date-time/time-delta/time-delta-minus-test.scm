(import (liii check)
        (liii date-time time-delta))

(check-set-mode! 'report-failed)

;; time-delta-
;; 计算 time-delta 的差值或取负。
;;
;; 语法
;; ----
;; (time-delta- dt)
;; (time-delta- dt1 dt2 ...)
;;
;; 参数
;; ----
;; dt, dt1, dt2 ... : time-delta?
;;
;; 返回值
;; ----
;; time-delta?
;;
;; 示例
;; ----
;; (time-delta- (days-delta 5) (days-delta 2)) => (days-delta 3)
;;
;; 错误处理
;; --------
;; type-error 当任一参数不是 time-delta 对象时抛出错误。

(let ((dt (time-delta- (days-delta 5) (days-delta 2))))
  (check (time-delta-days dt) => 3))

(let ((dt (time-delta- (hours-delta 10))))
  (check (time-delta-hours dt) => -10))

(check-catch 'type-error (time-delta- 10))

(check-report)
