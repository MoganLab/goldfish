(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

;; gcd
;; 用于计算给定整数的最大公约数。
;;
;; 语法
;; ----
;; (gcd integer ...)
;;
;; 参数
;; ----
;; integer : integer? - 整数。接受零个、一个或多个参数。
;;
;; 返回值
;; ------
;; integer?
;; 返回所有参数的最大公约数。无参数时返回0，单参数时返回该参数的绝对值。
;;
;; 特殊规则
;; --------
;; - 无参数时返回0
;; - 参数中包含0时，忽略0值
;; - 负数会被取绝对值处理
;; - 多个参数按顺序计算最大值公约数
;;
;; 错误处理
;; ----
;; wrong-type-arg
;; 当参数不是整数时抛出错误。

(check (gcd) => 0)
(check (gcd 0) => 0)
(check (gcd 1) => 1)
(check (gcd 2) => 2)
(check (gcd -1) => 1)

(check (gcd 0 1) => 1)
(check (gcd 1 0) => 1)
(check (gcd 1 2) => 1)
(check (gcd 1 10) => 1)
(check (gcd 2 10) => 2)
(check (gcd -2 10) => 2)

(check (gcd 2 3 4) => 1)
(check (gcd 2 4 8) => 2)
(check (gcd -2 4 8) => 2)
(check (gcd 15 20 25) => 5)
(check (gcd 6 9 12 15) => 3)
(check (gcd 0 4 6) => 2)
(check (gcd 1 2 3 4 5) => 1)
(check (gcd 12 18) => 6)
(check (gcd 18 12) => 6)
(check (gcd 21 35) => 7)
(check (gcd 0 5) => 5)
(check (gcd 15 0) => 15)
(check (gcd -6 8) => 2)
(check (gcd 12 -9) => 3)

(check-catch 'wrong-type-arg (gcd 1.5))
(check-catch 'wrong-type-arg (gcd 2.3))
(check-catch 'wrong-type-arg (gcd 1+i))
(check-catch 'wrong-type-arg (gcd 'hello))
(check-catch 'wrong-type-arg (gcd 1 2+i 3))
(check-catch 'wrong-type-arg (gcd 1.5 2.5))

(check-report)
