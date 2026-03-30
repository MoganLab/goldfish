(import (liii check)
        (liii base)
        (liii list)
        (liii case)
        (liii lang)
        (liii error)
        (liii os))

(check-set-mode! 'report-failed)

;; square
;; 计算给定数值的平方。
;;
;; 语法
;; ----
;; (square x)
;;
;; 参数
;; ----
;; x : number?
;; 数值。支持整数、有理数、浮点数等各种数值类型。
;;
;; 返回值
;; ------
;; 返回x的平方值，保持与输入相同的数值类型精度。
;; 对于整数，返回精确的平方值；对于浮点数，返回浮点数平方值。
;;
;; 错误处理
;; --------
;; wrong-type-arg
;; 当参数不是数值时抛出错误。

;; square测试
(check (square 2) => 4)
(check (square 0) => 0)
(check (square -2) => 4)
(check (square 5) => 25)
(check (square -5) => 25)
(check (square 1/2) => 1/4)
(check (square -1/3) => 1/9)
(check (square 2.5) => 6.25)
(check (square 0.0) => 0.0)
(check (square 10) => 100)
(check (square 1+2i) => -3+4i)
(check-catch 'wrong-type-arg (square "a"))


;; 补充square边界测试
(check (square 1) => 1)
(check (square -1) => 1)
(check (square 1000) => 1000000)
(check (square 1/100) => 1/10000)
(check (square 0.001) => 0.000001)

(check-report)
