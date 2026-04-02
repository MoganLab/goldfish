(import (liii check)) ;import

(check-set-mode! 'report-failed)

;; check-true
;; 断言表达式结果为 `#t`。
;;
;; 语法
;; ----
;; (check-true expr)

(check-true (number? 42))
(check-true (zero? 0))
(check-true (not #f))

(check-report)
