(import (liii check)
        (liii date-time time-delta))

(check-set-mode! 'report-failed)

;; time-delta-days
;; 获取 time-delta 的日增量。
;;
;; 语法
;; ----
;; (time-delta-days dt)
;;
;; 参数
;; ----
;; dt : time-delta?
;;
;; 返回值
;; ----
;; integer? 日分量。
;;
;; 示例
;; ----
;; (time-delta-days (days-delta 10)) => 10
;;
;; 错误处理
;; --------
;; type-error 当参数不是 time-delta 对象时抛出错误。

(check (time-delta-days (days-delta 10)) => 10)
(check (time-delta-days (days-delta -5)) => -5)

(check-catch 'type-error (time-delta-days 10))

(check-report)
