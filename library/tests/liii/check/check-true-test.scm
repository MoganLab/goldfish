;; check-true
;; 断言表达式求值结果为 #t。
;;
;; 语法
;; ----
;; (check-true expr)
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

(check-true (number? 42))
(check-true (zero? 0))
(check-true (not #f))

(check-report)
