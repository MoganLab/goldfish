(import (liii check)) ;import

(check-set-mode! 'report-failed)

;; check-catch
;; 断言表达式会抛出指定错误标识。
;;
;; 语法
;; ----
;; (check-catch error-id expr)

(check-catch 'wrong-type-arg (car 123))
(check-catch 'wrong-number-of-args (car))
(check-catch 'wrong-number-of-args (car '(1 2) '(3 4)))

(check-report)
