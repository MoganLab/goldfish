(import (liii check)
        (liii date-time time-delta))

(check-set-mode! 'report-failed)

;; time-delta-years
;; 获取 time-delta 的年增量。
;;
;; 语法
;; ----
;; (time-delta-years dt)
;;
;; 参数
;; ----
;; dt : time-delta?
;;
;; 返回值
;; ----
;; integer? 年分量。
;;
;; 示例
;; ----
;; (time-delta-years (years-delta 5)) => 5
;;
;; 错误处理
;; --------
;; type-error 当参数不是 time-delta 对象时抛出错误。

(check (time-delta-years (years-delta 5)) => 5)
(check (time-delta-years (years-delta -3)) => -3)

(check-catch 'type-error (time-delta-years 5))
(check-catch 'type-error (time-delta-years "5"))

(check-report)
