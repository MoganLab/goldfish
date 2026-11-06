(import (liii check)
        (liii date-time time-delta))

(check-set-mode! 'report-failed)

;; time-delta-months
;; 获取 time-delta 的月增量。
;;
;; 语法
;; ----
;; (time-delta-months dt)
;;
;; 参数
;; ----
;; dt : time-delta?
;;
;; 返回值
;; ----
;; integer? 月分量。
;;
;; 示例
;; ----
;; (time-delta-months (months-delta 6)) => 6
;;
;; 错误处理
;; --------
;; type-error 当参数不是 time-delta 对象时抛出错误。

(check (time-delta-months (months-delta 6)) => 6)
(check (time-delta-months (months-delta -2)) => -2)

(check-catch 'type-error (time-delta-months 6))

(check-report)
