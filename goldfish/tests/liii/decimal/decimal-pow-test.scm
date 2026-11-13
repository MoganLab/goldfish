;; decimal-pow
;; 计算 decimal 的 n 次幂。
;;
;; 语法
;; ----
;; (decimal-pow n exponent)
;; (decimal-pow n exponent scale)
;; (decimal-pow n exponent scale rounding-mode)
;;
;; 参数
;; ----
;; n : decimal?
;;   底数。
;; exponent : exact-nonnegative-integer?
;;   指数，必须为非负整数。
;; scale : integer?
;;   可选参数，目标 scale。如果提供，结果会被舍入到该 scale。
;; rounding-mode : symbol?
;;   可选参数，舍入模式。默认为 decimal-round-unnecessary。
;;
;; 返回值
;; ----
;; decimal?
;;   n 的 exponent 次幂。
;;
;; 说明
;; ----
;; 如果没有提供 scale，则结果的 scale 为 n 的 scale 乘以 exponent。
;;
;; 错误处理
;; ----
;; type-error
;;   当 n 不是 decimal 或 exponent 不是整数时抛出。
;; value-error
;;   当 exponent 为负数时抛出。

(import (liii check)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)

(define (d str)
  (string->decimal str)
) ;define
;; 指数 0：任何数的 0 次幂为 1
(check (decimal->string (decimal-pow (d "2") 0)) => "1")
(check (decimal->string (decimal-pow (d "0") 0)) => "1")
(check (decimal->string (decimal-pow (d "1.5") 0)) => "1")

;; 指数 1：返回原值
(check (decimal->string (decimal-pow (d "2") 1)) => "2")
(check (decimal->string (decimal-pow (d "1.5") 1)) => "1.5")
(check (decimal->string (decimal-pow (d "-3") 1)) => "-3")

;; 整数幂运算
(check (decimal->string (decimal-pow (d "2") 10)) => "1024")
(check (decimal->string (decimal-pow (d "3") 5)) => "243")
(check (decimal->string (decimal-pow (d "10") 6)) => "1000000")

;; 小数幂运算（scale 相乘）
(check (decimal->string (decimal-pow (d "1.1") 2)) => "1.21")
(check (decimal->string (decimal-pow (d "1.01") 2)) => "1.0201")
(check (decimal->string (decimal-pow (d "2.5") 3)) => "15.625")

;; 负数底数
(check (decimal->string (decimal-pow (d "-2") 2)) => "4")
(check (decimal->string (decimal-pow (d "-2") 3)) => "-8")
(check (decimal->string (decimal-pow (d "-1.5") 2)) => "2.25")

;; 指定 scale
(check (decimal->string (decimal-pow (d "2") 10 0)) => "1024")
(check (decimal->string (decimal-pow (d "1.1") 2 1 decimal-round-half-up)) => "1.2")
(check (decimal->string (decimal-pow (d "1.5") 3 2 decimal-round-half-up))
       => "3.38")
(check (decimal->string (decimal-pow (d "2.5") 3 1 decimal-round-half-up))
       => "15.6")

;; 大指数
(check (decimal->string (decimal-pow (d "2") 20)) => "1048576")
(check (decimal->string (decimal-pow (d "1.01") 100 2 decimal-round-half-up))
       => "2.70")

;; 错误处理
(check-catch 'type-error (decimal-pow 2 3))
(check-catch 'type-error (decimal-pow (d "2") 1.5))
(check-catch 'type-error (decimal-pow (d "2") 3 1.5))
(check-catch 'value-error (decimal-pow (d "2") -1))
(check-catch 'value-error (decimal-pow (d "1.234") 2 1))  ;; rounding necessary

(check-report)
