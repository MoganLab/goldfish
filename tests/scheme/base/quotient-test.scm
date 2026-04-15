(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; quotient
;; 用于计算两个数的精确除法商（向零取整）。
;;
;; 语法
;; ----
;; (quotient dividend divisor)
;;
;; 参数
;; ----
;; dividend : real? - 被除数
;; divisor : real? - 除数，不能为零
;;
;; 返回值
;; ------
;; integer?
;; 返回一个整数，表示向零方向取整的商。
;;
;; 与floor-quotient的区别
;; -------------
;; quotient与floor-quotient的主要区别在于对负数除法的处理：
;; - quotient：向零取整（截断除法），如(quotient -11 2) => -5
;; - floor-quotient：向负无穷取整，如(floor-quotient -11 2) => -6
;;
;; 错误
;; ----
;; division-by-zero
;; 当除数为零时抛出错误。
;; wrong-type-arg
;; 当参数不是数字时抛出错误。
(check (quotient 11 2) => 5)
(check (quotient 11 -2) => -5)
(check (quotient -11 2) => -5)
(check (quotient -11 -2) => 5)
(check (quotient 10 3) => 3)
(check (quotient 10 -3) => -3)
(check (quotient -10 3) => -3)
(check (quotient -10 -3) => 3)
(check (quotient 0 5) => 0)
(check (quotient 0 -5) => 0)
(check (quotient 15 5) => 3)
(check (quotient -15 5) => -3)
(check (quotient 7 7) => 1)
(check (quotient 100 10) => 10)
(check (quotient 1 1) => 1)
(check (quotient -1 1) => -1)
(check (quotient 17 5) => 3)
(check (quotient -17 5) => -3)
(check (quotient 17 -5) => -3)
(check (quotient -17 -5) => 3)
(check-catch 'division-by-zero
  (quotient 11 0)
) ;check-catch
(check-catch 'division-by-zero
  (quotient 0 0)
) ;check-catch
(check (quotient 10.5 3.0) => 3)
(check (quotient 10.5 -3.0) => -3)
(check (quotient -10.5 3.0) => -3)
(check (quotient -10.5 -3.0) => 3)
(check-catch 'wrong-type-arg
  (quotient 1.0+1.0i 2)
) ;check-catch
(check-catch 'wrong-type-arg
  (quotient 'hello 2)
) ;check-catch
(check-catch 'wrong-number-of-args
  (quotient 10)
) ;check-catch
(check-catch 'wrong-number-of-args
  (quotient 5 3 2)
) ;check-catch
(check-report)