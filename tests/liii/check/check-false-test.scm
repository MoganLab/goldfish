(import (liii check))

(check-set-mode! 'report-failed)

;; check-false
;; 断言表达式结果为 `#f`。
;;
;; 语法
;; ----
;; (check-false expr)
;;
;; 使用场景
;; ----
;; 1. 验证谓词结果应为 `#f` 的场景。
;; 2. 验证某个条件分支当前不应命中。
;; 3. 验证某个错误状态或标志尚未出现。

(check-false (number? 'hello))
(check-false (zero? 1))
(check-false (boolean? 123))

(check-report)
