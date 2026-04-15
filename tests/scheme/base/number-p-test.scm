(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; number?
;; 判断一个对象是否是数（包括整数、浮点数、有理数、复数）。
;;
;; 语法
;; ----
;; (number? obj)
;;
;; 参数
;; ----
;; obj : any
;; 任意类型的对象。
;;
;; 返回值
;; -----
;; boolean?
;; 如果 obj 是数值类型（整数、浮点数、有理数、复数）返回 #t，否则返回 #f。
;;
;; 错误
;; ----
;; 无错误情况。
(check-true (number? 123))
(check-true (number? 123.456))
(check-true (number? 1/2))
(check-true (number? 1.0+2.0i))
(check-false (number? "123"))
(check-false (number? #t))
(check-false (number? 'symbol))
(check-false (number? '(1 2 3)))
(check-report)
