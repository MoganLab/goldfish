;; check-false
;; 断言表达式求值结果为 #f。
;;
;; 语法
;; ----
;; (check-false expr)
;;
;; 参数
;; ----
;; expr : any
;; 要求值并验证的表达式。
;;
;; 返回值
;; ----
;; unspecified
;; 断言结果会被纳入 check-report 统计。

(import (liii check))

(check-set-mode! 'report-failed)

(check-false (number? 'hello))
(check-false (zero? 1))
(check-false (boolean? 123))

(check-report)
