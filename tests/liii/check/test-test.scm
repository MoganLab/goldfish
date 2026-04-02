(import (liii check)) ;import

(check-set-mode! 'report-failed)

;; test
;; `check` 的简写别名，用于精确断言。
;;
;; 语法
;; ----
;; (test actual expected)
;;
;; 参数
;; ----
;; actual : any
;; 要求值的表达式。
;; expected : any
;; 期望结果。
;;
;; 使用场景
;; ----
;; 1. 在短小测试里使用更简洁的断言写法。
;; 2. 在教学示例中降低断言语法负担。
;; 3. 为一组简单等值比较快速补测试。

(test (+ 2 3) 5)
(test '(x y) '(x y))
(test (boolean? #t) #t)

(check-report)
