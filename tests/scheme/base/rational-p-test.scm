(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; rational?
;; 判断一个对象是否是有理数（包括整数、有理数）。
;;
;; 语法
;; ----
;; (rational? obj)
;;
;; 参数
;; ----
;; obj : any
;; 任意类型的对象。
;;
;; 返回值
;; -----
;; boolean?
;; 如果 obj 是数值类型（整数、有理数）返回 #t，否则返回 #f。
;;
;; 错误
;; ----
;; 无错误情况。
(check-true (rational? 123))
(check-true (rational? 1/2))
(check-false (rational? 123.456))
(check-false (rational? 1.0+2.0i))
(check-false (rational? "123"))
(check-false (rational? #t))
(check-false (rational? 'symbol))
(check-false (rational? '(1 2 3)))
(check-report)
