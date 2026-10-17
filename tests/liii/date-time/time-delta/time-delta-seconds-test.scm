(import (liii check)
        (liii date-time time-delta))

(check-set-mode! 'report-failed)

;; time-delta-seconds
;; 获取 time-delta 的秒增量（精确有理数或整数）。
;;
;; 语法
;; ----
;; (time-delta-seconds dt)
;;
;; 参数
;; ----
;; dt : time-delta?
;;
;; 返回值
;; ----
;; rational? 秒分量。
;;
;; 示例
;; ----
;; (time-delta-seconds (seconds-delta 30)) => 30
;; (time-delta-seconds (seconds-delta 1/2)) => 1/2
;;
;; 错误处理
;; --------
;; type-error 当参数不是 time-delta 对象时抛出错误。

(check (time-delta-seconds (seconds-delta 30)) => 30)
(check (time-delta-seconds (seconds-delta 1/2)) => 1/2)
(check (time-delta-seconds (seconds-delta -10)) => -10)

(check-catch 'type-error (time-delta-seconds 30))

(check-report)
