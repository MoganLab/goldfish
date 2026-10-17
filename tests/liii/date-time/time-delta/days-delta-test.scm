(import (liii check)
        (liii date-time time-delta))

(check-set-mode! 'report-failed)

;; days-delta
;; 创建仅包含天数增量的 time-delta 对象。
;;
;; 语法
;; ----
;; (days-delta n)
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
;; (time-delta-days (days-delta 15)) => 15
;;
;; 错误处理
;; --------
;; type-error 当参数不是精确整数时抛出错误。

(let ((dt (days-delta 15)))
  (check-true (time-delta? dt))
  (check (time-delta-days dt) => 15))

(check-catch 'type-error (days-delta 1.1))

(check-report)
