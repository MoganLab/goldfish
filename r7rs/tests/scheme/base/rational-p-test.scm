;; rational?
;; 判断一个对象是否是有理数（包括整数、有理数）。
;;
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
;; ----
;; boolean?
;; 如果 obj 是数值类型（整数、有理数）返回 #t，否则返回 #f。
;;
;; 错误处理
;; ----
;; 无错误情况。

(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

(check-true (rational? 123))
(check-true (rational? 1/2))
;; R7RS 6.2.6：有限浮点数属于有理数
(check-true (rational? 123.456))
(check-true (rational? 9007199254740992.0))
(check-false (rational? +inf.0))
(check-false (rational? -inf.0))
(check-false (rational? +nan.0))
(check-false (rational? 1.0+2.0i))
(check-false (rational? "123"))
(check-false (rational? #t))
(check-false (rational? 'symbol))
(check-false (rational? '(1 2 3)))

(check-report)
