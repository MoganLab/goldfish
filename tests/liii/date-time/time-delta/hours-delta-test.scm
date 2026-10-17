(import (liii check)
        (liii date-time time-delta))

(check-set-mode! 'report-failed)

;; hours-delta
;; 创建仅包含小时增量的 time-delta 对象。
;;
;; 语法
;; ----
;; (hours-delta n)
;;
;; 参数
;; ----
;; n : integer? 精确整数。
;;
;; 返回值
;; ----
;; time-delta?
;;
;; 示例
;; ----
;; (time-delta-hours (hours-delta 8)) => 8
;;
;; 错误处理
;; --------
;; type-error 当参数不是精确整数时抛出错误。

(let ((dt (hours-delta 8)))
  (check-true (time-delta? dt))
  (check (time-delta-hours dt) => 8))

(check-catch 'type-error (hours-delta "8"))

(check-report)
