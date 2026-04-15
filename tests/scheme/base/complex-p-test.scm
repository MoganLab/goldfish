(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; complex?
;; 判断一个对象是否是复数（包括整数、浮点数、有理数、复数）。
;;
;; 语法
;; ----
;; (complex? obj)
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
(check-true (complex? 1.0+2.0i))
(check-true (complex? 123))
(check-true (complex? 123.456))
(check-true (complex? 1/2))
(check-false (complex? "123"))
(check-false (complex? #t))
(check-false (complex? 'symbol))
(check-false (complex? '(1 2 3)))
(check-report)