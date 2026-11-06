;; check-catch
;; 断言表达式求值时会抛出指定的错误标识。
;;
;; 语法
;; ----
;; (check-catch error-id expr)
;;
;; 参数
;; ----
;; error-id : symbol?
;; 期望捕获的错误标识符号（如 'type-error, 'wrong-number-of-args 等）。
;; expr : any
;; 预期会抛出错误的待求值表达式。
;;
;; 返回值
;; ----
;; unspecified
;; 断言结果会被纳入 check-report 统计。

(import (liii check))

(check-set-mode! 'report-failed)

(check-catch 'type-error (car 123))
(check-catch 'wrong-number-of-args (car))
(check-catch 'wrong-number-of-args (car '(1 2) '(3 4)))

(check-report)
