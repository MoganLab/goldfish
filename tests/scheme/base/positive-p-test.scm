(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; positive?
;; 判断一个对象是否是正数。
;;
;; 语法
;; ----
;; (positive? obj)
;;
;; 参数
;; ----
;; obj : any
;; 实数。
;;
;; 返回值
;; -----
;; boolean?
;; 如果 obj 是实数类型，当其为正数时返回 #t，否则返回 #f。
;;
;; 错误
;; ----
;; type-error
;; 如果参数不是实数类型（包括复数和非数值类型）
(check-true (positive? 1))
(check-true (positive? 0.1))
(check-true (positive? 1/2))
(check-true (positive? +inf.0))
(check-true (positive? 1.0))
(check-false (positive? 0))
(check-false (positive? -1))
(check-false (positive? -1.1))
(check-false (positive? -1/2))
(check-false (positive? -inf.0))
(check-false (positive? +nan.0))
(check-catch 'type-error (positive? 1.0+1.0i))
(check-catch 'type-error (positive? #\A))
(check-catch 'type-error (positive? #t))
(check-catch 'type-error (positive? "not-a-number"))
(check-catch 'type-error (positive? 'symbol))
(check-catch 'type-error (positive? '(1 2 3)))
(check-report)
