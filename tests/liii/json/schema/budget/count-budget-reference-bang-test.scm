(import (liii check) (liii json schema budget))

(check-set-mode! 'report-failed)

;; count-budget-reference!
;; 累加一次引用解析并返回新的引用次数。
;;
;; 语法
;; ----
;; (count-budget-reference! budget)
;;
;; 参数
;; ----
;; budget : budget
;; 由 make-resource-budget 创建的预算记录。
;;
;; 返回值
;; ----
;; integer
;; 累加后的引用解析次数。
;;
;; 注意
;; ----
;; 次数超过 max-references 上限时抛出 json-schema-budget-exceeded，
;; 用于阻止递归引用造成的无限展开。
;;
;; 示例
;; ----
;; (count-budget-reference! budget) => 1
;;
;; 错误处理
;; ----
;; json-schema-budget-exceeded 当引用次数超过上限时。

(define budget (make-resource-budget 100 100 2))

(check (count-budget-reference! budget) => 1)
(check (count-budget-reference! budget) => 2)
(check (budget-references budget) => 2)

;; 超过上限时抛出预算异常
(check-catch 'json-schema-budget-exceeded (count-budget-reference! budget))
(check (budget-references budget) => 3)

(check-report)
