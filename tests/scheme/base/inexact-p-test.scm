(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; inexact?
;; 用于判断一个数值是否为不精确值。
;;
;; 语法
;; ----
;; (inexact? obj)
;;
;; 参数
;; ----
;; obj : number?
;; 任何数值类型的对象
;;
;; 返回值
;; -----
;; boolean?
;; 如果 obj 是不精确数（不精确的浮点数、运算结果中的不精确部分、复数的任何部分是不精确的等）返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; 1. 整数和有理数（精确分数）通常返回 #f，表示它们是精确的
;; 2. 浮点数和运算中涉及不精确数的表达式通常返回 #t
;; 3. 对于复数，如果实部或虚部任何一部分是不精确的，则返回 #t
;; 4. 特殊数值如无穷大和NaN返回 #t
;; 5. 精确浮点数（使用精确前缀）返回 #f
;;
;; 错误处理
;; --------
;; wrong-type-arg
;; 如果参数不是数字类型，抛出错误。
;; 基本测试
(check-false (inexact? 42))
(check-false (inexact? 3/4))
(check-true (inexact? 3.14))
(check-true (inexact? 1000.0))
(check-true (inexact? 1.0+2.0i))
(check-true (inexact? +inf.0))
(check-true (inexact? -inf.0))
(check-true (inexact? +nan.0))
;; 精确值测试
(check-false (inexact? 0))
(check-false (inexact? 1))
(check-false (inexact? -1))
(check-false (inexact? 1000000))
(check-false (inexact? -1000000))
(check-false (inexact? 1/2))
(check-false (inexact? 1/3))
(check-false (inexact? 5/3))
(check-false (inexact? -1/2))
(check-false (inexact? -5/7))
;; 不精确值测试
(check-true (inexact? 0.0))
(check-true (inexact? 1.0))
(check-true (inexact? -1.0))
(check-true (inexact? 0.5))
(check-true (inexact? 3.14159))
(check-true (inexact? -3.14159))
(check-true (inexact? 1e+10))
(check-true (inexact? 1.0))
;; 运算结果测试
(check-true (inexact? (+ 1.0 2.0)))
(check-false (inexact? (+ 1 2)))
(check-true (inexact? (+ 1 2.0)))
(check-false (inexact? (* 1/2 4)))
(check-true (inexact? (* 0.5 4)))
;; 边界测试
(check-true (inexact? 1.7976931348623157e+308)
) ;check-true
(check-true (inexact? 2.2250738585072014e-308)
) ;check-true
;; 错误测试
(check-catch 'wrong-type-arg
  (inexact? "not a number")
) ;check-catch
(check-catch 'wrong-type-arg
  (inexact? 'symbol)
) ;check-catch
(let ((zero-int 0))
  (check-true (and (integer? zero-int)
                (zero? zero-int)
              ) ;and
  ) ;check-true
) ;let
(let ((zero-exact (- 1/2 1/2)))
  (check-true (and (exact? zero-exact)
                (zero? zero-exact)
              ) ;and
  ) ;check-true
) ;let
(let ((zero-inexact 0.0))
  (check-true (and (inexact? zero-inexact)
                (zero? zero-inexact)
              ) ;and
  ) ;check-true
) ;let
(check-false (zero? 1.0+1.0i))
(check-false (zero? 3))
(check-catch 'wrong-type-arg
  (zero? #\A)
) ;check-catch
(check-catch 'wrong-type-arg (zero? #t))
(check-catch 'wrong-type-arg (zero? #f))
(check-report)