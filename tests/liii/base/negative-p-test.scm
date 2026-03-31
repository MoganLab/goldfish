(import (liii check)
        (liii base)
        (liii list)
        (liii case)
        (liii error)
        (liii os))

(check-set-mode! 'report-failed)

;; negative?
;; 判断一个对象是否是负数。
;;
;; 语法
;; ----
;; (negative? obj)
;;
;; 参数
;; ----
;; obj : real?
;; 实数。
;;
;; 返回值
;; -----
;; boolean?
;; 如果 obj 是实数类型，当其为负数时返回 #t，否则返回 #f。
;;
;; 错误
;; ----
;; wrong-type-arg
;; 如果参数不是实数类型（包括复数和非数值类型）

(check-true (negative? -1))
(check-true (negative? -0.1))
(check-true (negative? -1/2))
(check-true (negative? -inf.0))
(check-true (negative? -1+0i))

(check-false (negative? 0))
(check-false (negative? 1))
(check-false (negative? 1.1))
(check-false (negative? 1/2))
(check-false (negative? +inf.0))
(check-false (negative? -nan.0))

(check-catch 'wrong-type-arg (negative? -1-1i))
(check-catch 'wrong-type-arg (negative? #\A))
(check-catch 'wrong-type-arg (negative? #t))
(check-catch 'wrong-type-arg (negative? "not-a-number"))
(check-catch 'wrong-type-arg (negative? 'symbol))
(check-catch 'wrong-type-arg (negative? '(1 2 3)))

(check-report)
