(import (liii check)
        (liii date-time time-delta))

(check-set-mode! 'report-failed)

;; weeks-delta
;; 创建仅包含周数增量的 time-delta 对象。
;;
;; 语法
;; ----
;; (weeks-delta n)
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
;; (time-delta-weeks (weeks-delta 2)) => 2
;;
;; 错误处理
;; --------
;; type-error 当参数不是精确整数时抛出错误。

(let ((dt (weeks-delta 2)))
  (check-true (time-delta? dt))
  (check (time-delta-weeks dt) => 2))

(check-catch 'type-error (weeks-delta #f))

(check-report)
