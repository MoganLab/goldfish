(import (liii check)
        (scheme inexact)
) ;import

(check-set-mode! 'report-failed)

;; asin
;; 计算给定值的反正弦值。
;;
;; 语法
;; ----
;; (asin x)
;;
;; 参数
;; ----
;; x : real?
;; 必须在区间[-1, 1]内的实数，表示sin函数的值。
;;
;; 返回值
;; ------
;; real?
;; 返回x的反正弦值（arcsin），范围在[-π/2, π/2]内。
;;
;; 注意
;; ----
;; 1. 计算反正弦函数arcsin(x)
;; 2. 支持整数、有理数、浮点数等各种数值类型
;; 3. 当|x| > 1时，返回复数值
;; 4. 返回值精确度与输入值类型保持一致
;;
;; 示例
;; ----
;; (asin 0) => 0.0
;; (asin 1) => 1.5707963267948966 (π/2)
;; (asin -1) => -1.5707963267948966 (-π/2)
;;
;; 错误处理
;; ------
;; wrong-type-arg
;; 当参数不是实数或超出范围时抛出错误。
;; wrong-number-of-args
;; 当参数数量不为1时抛出错误。

;; asin 基本测试
(check (asin 0) => 0)
(check (asin 1) => 1.5707963267948966)
(check (asin -1) => -1.5707963267948966)

;; 特殊值测试
(check (asin (/ (sqrt 2) 2)) => 0.7853981633974484)
(check (asin (/ (sqrt 3) 2)) => 1.0471975511965976)

;; 边界测试
(check (asin 0.000001) => 1.0000000000001666e-6)

;; 有理数测试
(check (asin 2/3) => 0.7297276562269664)

;; 错误处理测试
(check-catch 'wrong-type-arg (asin "hello"))
(check-catch 'wrong-number-of-args (asin))
(check-catch 'wrong-number-of-args (asin 1 2))

(check-report)
