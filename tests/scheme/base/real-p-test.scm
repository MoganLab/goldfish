(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; real?
;; 判断一个对象是否实数（包括整数、浮点数、有理数）。
;;
;; 语法
;; ----
;; (real? obj)
;;
;; 参数
;; ----
;; obj : any
;; 任意类型的对象。
;;
;; 返回值
;; -----
;; boolean?
;; 如果 obj 是数值类型（整数、浮点数、有理数）返回 #t，否则返回 #f。
;;
;; 错误
;; ----
;; 无错误情况。
(check-true (real? 123))
(check-true (real? 123.456))
(check-true (real? 1/2))
(check-false (real? 1.0+2.0i))
(check-false (real? "123"))
(check-false (real? #t))
(check-false (real? 'symbol))
(check-false (real? '(1 2 3)))
(check-report)
