(import (liii check))

(check-set-mode! 'report-failed)

;; check-catch
;; 断言表达式会抛出指定错误标识。
;;
;; 语法
;; ----
;; (check-catch error-id expr)
;;
;; 使用场景
;; ----
;; 1. 验证非法参数类型会触发预期错误。
;; 2. 验证参数个数不正确时的错误分支。
;; 3. 为边界条件和失败路径补充回归测试。

(check-catch 'wrong-type-arg (car 123))
(check-catch 'wrong-number-of-args (car))
(check-catch 'wrong-number-of-args (car '(1 2) '(3 4)))

(check-report)
