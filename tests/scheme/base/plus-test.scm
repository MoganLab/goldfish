(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; +
;; 计算所有给定数字的和。
;;
;; 语法
;; ----
;; (+ num ...)
;;
;; 参数
;; ----
;; num : number?
;; 任意个数字。
;;
;; 返回值
;; ------
;; number?
;; 如果没有参数，返回加法单位元 0
;; 否则，返回其所有参数的和
;;
;; 错误
;; ----
;; wrong-type-arg
;; 如果存在任何参数不是数字，抛出错误。
(check (+) => 0)
(check (+ 1) => 1)
(check (+ 1 2) => 3)
(check (+ 1 2 3) => 6)
(check (+ 1 2 3 4) => 10)
(check (+ 1.5 2.5) => 4.0)
(check (+ 0.1 0.2)
  =>
  0.30000000000000004
) ;check
(check (< (abs (- 3.3 (+ 1.1 2.2))) 1e-15)
  =>
  #t
) ;check
(check (+ 1/2 1/2) => 1)
(check (+ 1/3 1/2) => 5/6)
(check (+ 1/3 1/4 1/5) => 47/60)
(check (+ 1.0+1.0i 2.0+2.0i)
  =>
  3.0+3.0i
) ;check
(check (+ 3.0+2.0i 4.0-3.0i)
  =>
  7.0-1.0i
) ;check
(check (+ 1.0+1.0i 1) => 2.0+1.0i)
(check (+ 1.0+1.0i 1/2) => 1.5+1.0i)
(check (+ +inf.0 0.7) => +inf.0)
(check (+ -inf.0 7) => -inf.0)
(check (+ +inf.0 1.0+1.0i)
  =>
  +inf.0+1.0i
) ;check
(check (nan? (+ +nan.0 1)) => #t)
(check (nan? (+ +inf.0 -inf.0)) => #t)
(check (+ 1e+308 1e+308) => +inf.0)
(check (+ -1e+308 -1e+308) => -inf.0)
(check (+ 9223372036854775807 1)
  =>
  -9223372036854775808
) ;check
(check-catch 'wrong-type-arg
  (+ 'hello 7)
) ;check-catch
(check-catch 'wrong-type-arg
  (+ "world" 7)
) ;check-catch
(check-catch 'wrong-type-arg (+ #t 7))
(check-catch 'wrong-type-arg
  (+ '(1 3 5) 7)
) ;check-catch
(check-catch 'unbound-variable
  (+ 1.0+1.0i 2i)
) ;check-catch
(check-report)
