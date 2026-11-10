;; test
;; check 的简写别名，用于精确断言表达式求值结果。
;;
;; 语法
;; ----
;; (test actual expected)
;;
;; 参数
;; ----
;; actual : any
;; 要求值并验证结果的表达式。
;; expected : any
;; 期望结果值，使用 equal? 进行比较。
;;
;; 返回值
;; ----
;; unspecified
;; 断言结果会被纳入 check-report 统计。

(import (liii check))

(check-set-mode! 'report-failed)

(test (+ 2 3) 5)
(test '(x y) '(x y))
(test (boolean? #t) #t)

(check-report)
