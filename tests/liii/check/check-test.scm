(import (liii check)) ;import

(check-set-mode! 'report-failed)

;; check
;; 对表达式结果进行精确断言。
;;
;; 语法
;; ----
;; (check expr => expected)
;;
;; 参数
;; ----
;; expr : any
;; 要求值并检查结果的表达式。
;; expected : any
;; 期望结果，使用 `equal?` 进行比较。
;;
;; 返回值
;; ----
;; 无
;; 断言结果会被纳入 `check-report` 统计。
;;
;; 使用场景
;; ----
;; 1. 为纯函数和普通表达式编写基础断言。
;; 2. 验证列表、字符串、向量等结构化结果。
;; 3. 作为大多数单元测试的默认断言形式。

(check (+ 1 2) => 3)
(check '(a b c) => '(a b c))
(check (list 1 2 3) => '(1 2 3))

(check-report)
