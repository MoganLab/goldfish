(import (liii check)
        (liii date-time time-delta))

(check-set-mode! 'report-failed)

;; time-delta-minutes
;; 获取 time-delta 的分钟增量。
;;
;; 语法
;; ----
;; (time-delta-minutes dt)
;;
;; 参数
;; ----
;; dt : time-delta?
;;
;; 返回值
;; ----
;; integer? 分钟分量。
;;
;; 示例
;; ----
;; (time-delta-minutes (minutes-delta 45)) => 45
;;
;; 错误处理
;; --------
;; type-error 当参数不是 time-delta 对象时抛出错误。

(check (time-delta-minutes (minutes-delta 45)) => 45)
(check (time-delta-minutes (minutes-delta -15)) => -15)

(check-catch 'type-error (time-delta-minutes 45))

(check-report)
