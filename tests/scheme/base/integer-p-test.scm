(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; integer?
;; 判断一个对象是否是整数（包括整数）。
;;
;; 语法
;; ----
;; (integer? obj)
;;
;; 参数
;; ----
;; obj : any
;; 任意类型的对象。
;;
;; 返回值
;; -----
;; boolean?
;; 如果 obj 是数值类型（整数）返回 #t，否则返回 #f。
;;
;; 错误
;; ----
;; 无错误情况。
(check-true (integer? 123))
(check-false (integer? 123.456))
(check-false (integer? 1/2))
(check-false (integer? 1.0+2.0i))
(check-false (integer? "123"))
(check-false (integer? #t))
(check-false (integer? 'symbol))
(check-false (integer? '(1 2 3)))
(check-report)