(import (liii check) (scheme inexact))
(check-set-mode! 'report-failed)
;; acos
;; 计算给定值的反余弦值。
;;
;; 语法
;; ----
;; (acos x)
;;
;; 参数
;; ----
;; x : real?
;; 必须在区间[-1, 1]内的实数，表示cos函数的值。
;;
;; 返回值
;; ------
;; real?
;; 返回x的反余弦值（arccos），范围在[0, π]内。
;;
;; 注意
;; ----
;; 1. 计算反余弦函数arccos(x)
;; 2. 支持整数、有理数、浮点数等各种数值类型
;; 3. 当|x| > 1时，返回复数值
;; 4. 返回值精确度与输入值类型保持一致
;;
;; 示例
;; ----
;; (acos 0) => 1.5707963267948966 (π/2)
;; (acos 1) => 0.0
;; (acos -1) => 3.141592653589793 (π)
;;
;; 错误处理
;; ------
;; wrong-type-arg
;; 当参数不是实数或超出范围时抛出错误。
;; wrong-number-of-args
;; 当参数数量不为1时抛出错误。
;; acos 基本测试
(check (acos 0) => 1.5707963267948966)
(check (acos 1) => 0)
(check (acos -1) => 3.141592653589793)
(check (acos -0.5) => 2.0943951023931957)
;; 特殊值测试
(check (acos (/ (sqrt 2) 2)) => 0.7853981633974483)
(check (acos (/ (sqrt 3) 2)) => 0.5235987755982989)
;; 边界测试
(check (acos 0.999999) => 0.0014142136802445852)
(check (acos 0.000001) => 1.5707953267948966)
(check (acos -0.999999) => 3.1401784399095485)
;; 有理数测试
(check (acos 3/4) => 0.7227342478134157)
(check (acos 2/3) => 0.8410686705679303)
;; 错误处理测试
(check-catch 'wrong-type-arg (acos "hello"))
(check-catch 'wrong-number-of-args (acos))
(check-catch 'wrong-number-of-args (acos 1 2))
(check-report)