(import (liii check)) ;import

(check-set-mode! 'report-failed)

;; check-true
;; 断言表达式结果为 `#t`。
;;
;; 语法
;; ----
;; (check-true expr)
;;
;; 使用场景
;; ----
;; 1. 验证谓词判断应成立的场景。
;; 2. 验证某个条件分支已经命中。
;; 3. 验证成功状态标志为 `#t`。

(check-true (number? 42))
(check-true (zero? 0))
(check-true (not #f))

(check-report)
