(import (liii check)
        (liii date-time time-delta))

(check-set-mode! 'report-failed)

;; time-delta-weeks
;; 获取 time-delta 的周增量。
;;
;; 语法
;; ----
;; (time-delta-weeks dt)
;;
;; 参数
;; ----
;; dt : time-delta?
;;
;; 返回值
;; ----
;; integer? 周分量。
;;
;; 示例
;; ----
;; (time-delta-weeks (weeks-delta 3)) => 3
;;
;; 错误处理
;; --------
;; type-error 当参数不是 time-delta 对象时抛出错误。

(check (time-delta-weeks (weeks-delta 3)) => 3)
(check (time-delta-weeks (weeks-delta -1)) => -1)

(check-catch 'type-error (time-delta-weeks 3))

(check-report)
