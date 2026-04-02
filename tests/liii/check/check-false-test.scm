(import (liii check)) ;import

(check-set-mode! 'report-failed)

;; check-false
;; 断言表达式结果为 `#f`。
;;
;; 语法
;; ----
;; (check-false expr)

(check-false (number? 'hello))
(check-false (zero? 1))
(check-false (boolean? 123))

(check-report)
