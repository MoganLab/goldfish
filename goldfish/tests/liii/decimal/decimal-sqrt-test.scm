;; decimal-sqrt
;; 计算 decimal 的平方根。
;;
;; 语法
;; ----
;; (decimal-sqrt n)
;; (decimal-sqrt n scale)
;; (decimal-sqrt n scale rounding-mode)
;;
;; 参数
;; ----
;; n : decimal?
;;   被开方数，必须为非负数。
;; scale : integer?
;;   可选参数，目标 scale。默认为 20。
;; rounding-mode : symbol?
;;   可选参数，舍入模式。默认为 decimal-round-half-up。
;;
;; 返回值
;; ----
;; decimal?
;;   n 的平方根。
;;
;; 说明
;; ----
;; 使用牛顿迭代法计算平方根。
;;
;; 错误处理
;; ----
;; type-error
;;   当 n 不是 decimal 时抛出。
;; value-error
;;   当 n 为负数时抛出。

(import (liii check)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)

(define (d str)
  (string->decimal str)
) ;define
;; 完全平方数（默认精度为20位）
(check (decimal->string (decimal-sqrt (d "4"))) => "2.00000000000000000000")
(check (decimal->string (decimal-sqrt (d "9"))) => "3.00000000000000000000")
(check (decimal->string (decimal-sqrt (d "16"))) => "4.00000000000000000000")
(check (decimal->string (decimal-sqrt (d "100"))) => "10.00000000000000000000")
(check (decimal->string (decimal-sqrt (d "0.25"))) => "0.50000000000000000000")
(check (decimal->string (decimal-sqrt (d "0.04"))) => "0.20000000000000000000")

;; 完全平方数（指定精度）
(check (decimal->string (decimal-sqrt (d "4") 0)) => "2")
(check (decimal->string (decimal-sqrt (d "9") 0)) => "3")
(check (decimal->string (decimal-sqrt (d "0.25") 1)) => "0.5")

;; 零值
(check (decimal->string (decimal-sqrt (d "0"))) => "0")
(check (decimal->string (decimal-sqrt (d "0") 5)) => "0.00000")

;; 指定 scale
(check (decimal->string (decimal-sqrt (d "2") 5)) => "1.41421")
(check (decimal->string (decimal-sqrt (d "2") 10)) => "1.4142135624")
(check (decimal->string (decimal-sqrt (d "3") 5)) => "1.73205")
(check (decimal->string (decimal-sqrt (d "5") 5)) => "2.23607")

;; 小数被开方数
(check (decimal->string (decimal-sqrt (d "0.5") 5)) => "0.70711")
(check (decimal->string (decimal-sqrt (d "2.25") 2)) => "1.50")

;; 边界值收敛防抖动测试（BUG-10）
(check (decimal->string (decimal-sqrt (d "0.0001") 10)) => "0.0100000000")
(check (decimal->string (decimal-sqrt (d "99998.0001") 10)) => "316.2246038815")

;; 大数
(check (decimal->string (decimal-sqrt (d "1000000") 0)) => "1000")
(check (decimal->string (decimal-sqrt (d "100000000") 0)) => "10000")

;; 舍入模式
(check (decimal->string (decimal-sqrt (d "2") 4 decimal-round-half-up))
       => "1.4142")
(check (decimal->string (decimal-sqrt (d "2") 4 decimal-round-down))
       => "1.4142")

;; 验证平方根的精度
;; 注意：平方后再开方会有精度损失，这里仅验证大致正确
(let ((sqrt-2 (decimal-sqrt (d "2") 10)))
  (check (string=? (decimal->string (decimal-pow sqrt-2 2 9 decimal-round-half-up))
                   "2.000000000")
         => #t)
) ;let

;; 错误处理
(check-catch 'type-error (decimal-sqrt 4))
(check-catch 'value-error (decimal-sqrt (d "-4")))
(check-catch 'value-error (decimal-sqrt (d "-1")))

(check-report)
