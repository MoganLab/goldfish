(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; min
;; 返回所有给定实数的最小值。
;;
;; 语法
;; ----
;; (min num ...)
;;
;; 参数
;; ----
;; num : real?
;; 任意个实数（大于等于1）。
;;
;; 返回值
;; ------
;; real?
;; 返回所给所有值的最小值。
;; 如果存在NaN，返回NaN。
;; 如果参数中存在不精确值，返回值也是不精确的，否则返回值是精确的
;;
;; 错误
;; ----
;; type-error
;; 如果存在任何参数不是实数，抛出错误。
;; wrong-number-of-args
;; 如果没有提供参数，抛出错误。
(check (min 7) => 7)
(check (min 3.5) => 3.5)
(check (min 1/3) => 1/3)
(check (min +inf.0) => +inf.0)
(check (min -inf.0) => -inf.0)
(check (nan? (min +nan.0)) => #t)
(check (min 7 3) => 3)
(check (min 3.0 7.0) => 3.0)
(check (min 3 7.0) => 3.0)
(check (min 7.0 3) => 3.0)
(check (min 1/2 1/3) => 1/3)
(check (min 1/3 2/3) => 1/3)
(check (min +inf.0 7) => 7.0)
(check (min 7 +inf.0) => 7.0)
(check (min -inf.0 7) => -inf.0)
(check (min 7 -inf.0) => -inf.0)
(check (nan? (min +nan.0 7)) => #t)
(check (nan? (min 7 +nan.0)) => #t)
(check (min 7 3 5) => 3)
(check (min 3.0 7.0 2.0) => 2.0)
(check (min 7 3.0 5) => 3.0)
(check (min 1/2 1/3 2/3) => 1/3)
(check (min +inf.0 7 3) => 3.0)
(check (min -inf.0 7 3) => -inf.0)
(check (nan? (min +nan.0 7 3)) => #t)
(check (nan? (min 7 +nan.0 3)) => #t)
(check (nan? (min +nan.0 +inf.0 -inf.0))
  =>
  #t
) ;check
(check (min 7 3.0 15/4) => 3.0)
(check (min 5.0 7/2 3) => 3.0)
(check (min +inf.0 7 39/4) => 7.0)
(check (min -inf.0 7 3.0) => -inf.0)
(check (nan? (min +nan.0 7.0 3)) => #t)
(check (nan? (min 7/3 +nan.0 3)) => #t)
(check-catch 'wrong-number-of-args
  (min)
) ;check-catch
(check-catch 'type-error (min 'hello 7))
(check-catch 'type-error
  (min "world" 7)
) ;check-catch
(check-catch 'type-error (min #t 7))
(check-catch 'type-error (min #f 7))
(check-catch 'type-error
  (min '(1 3 5) 7)
) ;check-catch
(check-catch 'type-error (min '() 7))
(check-catch 'type-error
  (min 1.0+2.0i 2)
) ;check-catch
(check-report)