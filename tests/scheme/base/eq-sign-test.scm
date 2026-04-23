(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; =
;; 比较多个数字是否数值相等。
;;
;; 语法
;; ----
;; (= num1 num2 ...)
;;
;; 参数
;; ----
;; num1, num2, ... : number?
;; 待比较的数字。
;;
;; 返回值
;; ------
;; boolean?
;; 如果所有参数数值相等则返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; 1. 至少需提供两个参数
;; 2. 支持整数、有理数、实数和复数的比较
;; 3. 精确数与不精确数比较时会考虑数值相等性
;;
;; 错误处理
;; --------
;; wrong-number-of-args
;; 参数数量不足两个时抛出。
;; wrong-type-arg
;; 任一参数不是数字时抛出。

;; 基础整数比较
(check (= 1 1) => #t)
(check (= 0 0) => #t)
(check (= 1 2) => #f)
(check (= 2 1) => #f)
(check (= -5 -5) => #t)
(check (= -5 5) => #f)

;; 多参数比较
(check (= 1 1 1) => #t)
(check (= 1 1 2) => #f)
(check (= 3 3 3 3) => #t)
(check (= 3 3 3 4) => #f)

;; 精确与不精确混合
(check (= 1 1.0) => #t)
(check (= 2 2.0 2) => #t)
(check (= 1 1.1) => #f)

;; 有理数比较
(check (= 1/2 0.5) => #t)
(check (= 3/4 0.75) => #t)
(check (= 1/10 0.1) => #t)
(check (= 1/3 0.3333333333333333) => #t)
(check (= 1/2 1/3) => #f)

;; 浮点数比较
(check (= 1.5 1.5) => #t)
(check (= 1.5 1.5) => #t)
(check (= 1.5 1.6) => #f)

;; 复数比较
(check (= 1.0+2.0i 1.0+2.0i) => #t)
(check (= 1.0+2.0i 1.0+3.0i) => #f)
(check (= 3.0+4.0i 3.0+4.0i) => #t)

;; 边界情况
(check (= 0 0.0) => #t)
(check (= -0.0 0.0) => #t)

;; 错误处理：参数不足
(check-catch 'wrong-number-of-args (=))
(check-catch 'wrong-number-of-args
  (= 1)
) ;check-catch

;; 错误处理：类型错误
(check-catch 'wrong-type-arg (= 1 'a))
(check-catch 'wrong-type-arg
  (= 1 "hello")
) ;check-catch
(check-catch 'wrong-type-arg
  (= 1 '(1 2))
) ;check-catch
(check-catch 'wrong-type-arg (= #t 1))

(check-report)
