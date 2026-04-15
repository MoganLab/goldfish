(import (liii check) (scheme inexact))
(check-set-mode! 'report-failed)
;; sqrt
;; 计算给定数值的平方根。
;;
;; 语法
;; ----
;; (sqrt z)
;;
;; 参数
;; ----
;; z : number?
;; 被开方数，可以是整数、有理数、浮点数或复数。
;;
;; 返回值
;; ------
;; number?
;; 返回z的平方根。当z为负数时，返回复数值。
;;
;; 注意
;; ----
;; 1. 计算平方根函数√z
;; 2. 支持整数、有理数、浮点数、复数等各种数值类型
;; 3. 当z为负数时，返回复数形式的平方根（如√-1 = 0+1i）
;; 4. 返回值精确度与输入值类型保持一致：
;; - 如果输入为精确值且结果可表示为精确值，则返回精确值
;; - 如果输入为不精确值，则返回不精确值
;; - 如果输入为负数，由于结果为复数，总是返回不精确值
;;
;; 示例
;; ----
;; (sqrt 9) => 3
;; (sqrt 25.0) => 5.0
;; (sqrt 2) => 1.4142135623730951 (近似值)
;; (sqrt -1) => 0.0+1.0i
;; (sqrt -4) => 0.0+2.0i
;; (sqrt 0) => 0
;; (sqrt 0.0) => 0.0
;; (sqrt 1/4) => 1/2
;;
;; 错误处理
;; ------
;; wrong-type-arg
;; 当参数不是数值时抛出错误。
;; wrong-number-of-args
;; 当参数数量不为1时抛出错误。
;; sqrt 基本测试
(check (sqrt 9) => 3)
(check (sqrt 25.0) => 5.0)
(check (sqrt 9/4) => 3/2)
(check (< (abs (- (sqrt 2.0) 1.4142135623730951)) 1e-10)
  =>
  #t
) ;check
;; sqrt 负数测试
(check (sqrt -1.0) => 0.0+1.0i)
(check (sqrt -1) => 0.0+1.0i)
(check (sqrt -4.0) => 0.0+2.0i)
(check (sqrt -4) => 0.0+2.0i)
(check (sqrt -2.25) => 0.0+1.5i)
;; sqrt 边界测试
(check (sqrt 0) => 0)
(check (sqrt 0.0) => 0.0)
(check (sqrt 1) => 1)
(check (sqrt 1.0) => 1.0)
;; sqrt 精度测试
(check (exact? (sqrt 4)) => #t)
(check (exact? (sqrt 4.0)) => #f)
(check (exact? (sqrt -1)) => #f)
(check (exact? (sqrt -1.0)) => #f)
;; sqrt 大型数值测试
(check (sqrt 10000) => 100)
(check (sqrt 1000000.0) => 1000.0)
;; 错误处理测试
(check-catch 'wrong-type-arg (sqrt "hello"))
(check-catch 'wrong-type-arg (sqrt 'symbol))
(check-catch 'wrong-number-of-args (sqrt))
(check-catch 'wrong-number-of-args (sqrt 1 2))
(check-report)