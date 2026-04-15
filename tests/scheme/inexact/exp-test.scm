(import (liii check)
        (scheme inexact)
        (liii os)
) ;import

(check-set-mode! 'report-failed)

;; exp
;; 计算指数函数 e^n。
;;
;; 语法
;; ----
;; (exp n)
;;
;; 参数
;; ----
;; n : number?
;; 可选的数值参数，指数值。
;;
;; 返回值
;; ------
;; number?
;; e的n次幂值。
;;
;; 注意
;; ----
;; 1. 计算自然指数函数
;; 2. e ≈ 2.718281828459045
;; 3. 支持整数、有理数、浮点数、复数等各种数值类型
;;
;; 错误处理
;; ------
;; wrong-type-arg
;; 当参数不是数时抛出错误。
;; wrong-number-of-args
;; 当参数数量不为1时抛出错误。

;; exp 基本测试
(check (exp 0) => 1)
(check (exp 1) => 2.718281828459045)
(check (exp -1) => 0.36787944117144233)
(check (exp 2) => 7.38905609893065)

;; exp 边界测试
(check (exp 10) => 22026.465794806718)
(check (exp -10) => 4.5399929762484854e-05)
(check (exp 0.5) => 1.6487212707001282)
(check (exp -0.5) => 0.6065306597126334)

;; 错误处理测试
(when (not (os-windows?))
  (check (exp 1+2i) => -1.1312043837568135+2.4717266720048188i)
) ;when

(check-catch 'wrong-type-arg (exp "hello"))
(check-catch 'wrong-number-of-args (exp))

(check-report)
