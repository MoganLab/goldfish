(import (liii check)
        (liii date-time time-delta))

(check-set-mode! 'report-failed)

;; time-delta-hours
;; 获取 time-delta 的小时增量。
;;
;; 语法
;; ----
;; (time-delta-hours dt)
;;
;; 参数
;; ----
;; dt : time-delta?
;;
;; 返回值
;; ----
;; integer? 小时分量。
;;
;; 示例
;; ----
;; (time-delta-hours (hours-delta 12)) => 12
;;
;; 错误处理
;; --------
;; type-error 当参数不是 time-delta 对象时抛出错误。

(check (time-delta-hours (hours-delta 12)) => 12)
(check (time-delta-hours (hours-delta -4)) => -4)

(check-catch 'type-error (time-delta-hours 12))

(check-report)
