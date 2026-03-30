(import (liii check)
        (liii base)
        (liii list)
        (liii case)
        (liii lang)
        (liii error)
        (liii os))

(check-set-mode! 'report-failed)

;; /
;; 除法函数，支持整数、浮点数、有理数和复数的除法运算。
;;
;; 语法
;; ----
;; (/ num ...)
;;
;; 参数
;; ----
;; num : number?
;; 任意个数字作为除数。如果没有参数，抛出错误；如果只有一个参数，则返回其倒数；如果有多个参数，则从第二个参数开始依次除第一个参数。
;;
;; 返回值
;; ------
;; number?
;; 如果没有参数，抛出错误；如果只有一个参数，返回其倒数；如果有多个参数，返回其左结合的商。
;;
;; 说明
;; ----
;; 支持任意精确度和混合类型的除法运算：
;; - 整数除法：精确计算，如果没有模除则保持精确
;; - 浮点数除法：可能出现精度误差
;; - 有理数除法：保持精确分数
;; - 复数除法：按复数除法规则计算
;;
;; 错误
;; ----
;; wrong-type-arg
;; 如果存在任何参数不是数字类型，则抛出此错误
;; division-by-zero
;; 除数为零时抛出此错误
;; wrong-number-of-args
;; 提供的参数个数与函数定义时所需的参数个数不匹配

(check (/ 5) => 1/5)
(check (/ 1) => 1)
(check (/ -1) => -1)
(check (/ 2.5) => 0.4)
(check (/ 0.1) => 10.0)
(check (/ 1/2) => 2)
(check (/ 4/3) => 3/4)
(check (/ 10 2) => 5)
(check (/ 3 4) => 3/4)
(check (< (abs (- (/ 1.2 0.3) 4.0)) 1e-15) => #t)
(check (/ 2/3 1/3) => 2)
(check (/ 6 4 2) => 3/4)
(check (/ 6 2 3) => 1)
(check (/ 120 2 3 4 5) => 1)

(check (/ 10 3) => 10/3)
(check (/ 1/2 1/3) => 3/2)
(check (/ 4/5 2/3) => 6/5)

(check (/ 1 1) => 1)
(check (/ 1+0i 1+0i) => 1.0)

(check (/ -10 5) => -2)
(check (/ 10 -5) => -2)
(check (/ -10 -5) => 2)

(check (/ 5.0 2.0) => 2.5)
(check (/ 1.0 3.0) => 0.3333333333333333)
(check (/ 1/2 0.5) => 1.0)
(check (/ 4/2 2) => 1)

(check-catch 'division-by-zero (/ 5 0))
(check-catch 'division-by-zero (/ 1 0 2))
(check-catch 'division-by-zero (/ 0))
(check-catch 'wrong-type-arg (/ 'hello 7))
(check-catch 'wrong-type-arg (/ "world" 7))
(check-catch 'wrong-type-arg (/ 5 #t))
(check-catch 'wrong-number-of-args (/))

(check-report)
