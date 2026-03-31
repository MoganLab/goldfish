(import (liii check)
        (liii base)
        (liii list)
        (liii case)
        (liii error)
        (liii os))

(check-set-mode! 'report-failed)

;; floor-quotient
;; 用于计算两个数的地板除法，返回向负无穷取整的商。
;;
;; 语法
;; ----
;; (floor-quotient dividend divisor)
;;
;; 参数
;; ----
;; dividend : number? - 被除数
;; divisor : number? - 除数，不能为零
;;
;; 返回值
;; ------
;; number?
;; 返回一个整数，表示向负无穷方向取整的商。
;;
;; 错误
;; ----
;; division-by-zero
;; 当除数为零时抛出错误。
;; wrong-type-arg
;; 当参数不是数字时抛出错误。

(check (floor-quotient 11 2) => 5)
(check (floor-quotient 11 -2) => -6)
(check (floor-quotient -11 2) => -6)
(check (floor-quotient -11 -2) => 5)

(check (floor-quotient 10 2) => 5)
(check (floor-quotient 10 -2) => -5)
(check (floor-quotient -10 2) => -5)
(check (floor-quotient -10 -2) => 5)

(check-catch 'division-by-zero (floor-quotient 11 0))
(check-catch 'division-by-zero (floor-quotient 0 0))
(check-catch 'wrong-type-arg (floor-quotient 1+i 2))

(check (floor-quotient 0 2) => 0)
(check (floor-quotient 0 -2) => 0)

(check (receive (q r) (floor/ 11 3) q) => 3)
(check (receive (q r) (floor/ 11 3) r) => 2)
(check (receive (q r) (floor/ 11 -3) q) => -4)
(check (receive (q r) (floor/ 11 -3) r) => -1)
(check (receive (q r) (floor/ -11 3) q) => -4)
(check (receive (q r) (floor/ -11 3) r) => 1)
(check (receive (q r) (floor/ -11 -3) q) => 3)
(check (receive (q r) (floor/ -11 -3) r) => -2)

(check (receive (q r) (floor/ 10 2) q) => 5)
(check (receive (q r) (floor/ 10 2) r) => 0)
(check (receive (q r) (floor/ 10 -2) q) => -5)
(check (receive (q r) (floor/ 10 -2) r) => 0)
(check (receive (q r) (floor/ -10 2) q) => -5)
(check (receive (q r) (floor/ -10 2) r) => 0)
(check (receive (q r) (floor/ -10 -2) q) => 5)
(check (receive (q r) (floor/ -10 -2) r) => 0)

(check (receive (q r) (floor/ 15 4) q) => 3)
(check (receive (q r) (floor/ 15 4) r) => 3)
(check (receive (q r) (floor/ 15 -4) q) => -4)
(check (receive (q r) (floor/ 15 -4) r) => -1)
(check (receive (q r) (floor/ -15 4) q) => -4)
(check (receive (q r) (floor/ -15 4) r) => 1)
(check (receive (q r) (floor/ -15 -4) q) => 3)
(check (receive (q r) (floor/ -15 -4) r) => -3)

(check (receive (q r) (floor/ 1 3) q) => 0)
(check (receive (q r) (floor/ 1 3) r) => 1)
(check (receive (q r) (floor/ 0 5) q) => 0)
(check (receive (q r) (floor/ 0 5) r) => 0)

(check-catch 'division-by-zero (floor/ 11 0))
(check-catch 'division-by-zero (floor/ 0 0))
(check-catch 'wrong-type-arg (floor/ 1+i 2))
(check-catch 'wrong-type-arg (floor/ 5 #t))

(check-report)
