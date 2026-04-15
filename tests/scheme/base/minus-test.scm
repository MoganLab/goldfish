(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; -
;; 计算所有给定数字的差。
;;
;; 语法
;; ----
;; (- num ...)
;;
;; 参数
;; ----
;; num : number?
;; 一个或多个数字。
;;
;; 返回值
;; ------
;; number?
;; 如果只有一个参数，返回其加法逆元（相反数）
;; 如果有多个参数，返回其所有参数左结合的差
;;
;; 错误
;; ----
;; wrong-type-arg
;; 如果存在任何参数不是数字，抛出错误。
;; wrong-number-of-args
;; 如果没有提供参数，抛出错误。
(check (- 5) => -5)
(check (- 2 1) => 1)
(check (- 7 2 1) => 4)
(check (- 10 1 2 3) => 4)
(check (- 1.5 0.5) => 1.0)
(check (< (abs (- 2.7 (- 6.98 2.5 1.78)))
         1e-15
       ) ;<
  =>
  #t
) ;check
(check (- 2/3 1/3) => 1/3)
(check (- 1/2 1/5 1/7) => 11/70)
(check (- 1 1/3) => 2/3)
(check (- 2.0+2.0i 1.0+1.0i)
  =>
  1.0+1.0i
) ;check
(check (- 2.0+1.0i 1) => 1.0+1.0i)
(check (- 1.0+1.0i 1/2) => 0.5+1.0i)
(check (- 3.0+4.0i 0.0+2.0i 1.0+1.0i)
  =>
  2.0+1.0i
) ;check
(check (- -inf.0 1) => -inf.0)
(check (- +inf.0 1) => +inf.0)
(check (- +inf.0 1.0+1.0i)
  =>
  +inf.0-1.0i
) ;check
(check (- 1 +inf.0) => -inf.0)
(check (- 1 -inf.0) => +inf.0)
(check (- 1.0+1.0i +inf.0)
  =>
  -inf.0+1.0i
) ;check
(check (nan? (- +nan.0 0.5)) => #t)
(check (nan? (- 1 2 +nan.0)) => #t)
(check (nan? (- +inf.0 +inf.0)) => #t)
(check-catch 'wrong-number-of-args (-))
(check-catch 'wrong-type-arg
  (- 'hello 7)
) ;check-catch
(check-catch 'wrong-type-arg
  (- "world" 7)
) ;check-catch
(check-catch 'wrong-type-arg (- #f 7))
(check-catch 'wrong-type-arg
  (- '(1 3 5) 7)
) ;check-catch
(check-catch 'unbound-variable
  (- 1.0+1.0i 2i)
) ;check-catch
(check-report)