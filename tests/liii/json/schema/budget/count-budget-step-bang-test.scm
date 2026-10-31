(import (liii check) (liii json schema budget))

(check-set-mode! 'report-failed)

;; count-budget-step!
;; 累加一次 Schema 节点评估并返回新的步数。
;;
;; 语法
;; ----
;; (count-budget-step! budget)
;;
;; 参数
;; ----
;; budget : budget
;; 由 make-resource-budget 创建的预算记录。
;;
;; 返回值
;; ----
;; integer
;; 累加后的步数。
;;
;; 注意
;; ----
;; 步数超过 max-steps 上限时抛出 json-schema-budget-exceeded。
;;
;; 示例
;; ----
;; (count-budget-step! budget) => 1
;;
;; 错误处理
;; ----
;; json-schema-budget-exceeded 当步数超过上限时。

(define budget (make-resource-budget 100 2 100))

(check (count-budget-step! budget) => 1)
(check (budget-steps budget) => 1)
(check (count-budget-step! budget) => 2)
(check (budget-steps budget) => 2)

;; 超过上限时抛出预算异常
(check-catch 'json-schema-budget-exceeded (count-budget-step! budget))
(check (budget-steps budget) => 3)

(check-report)
