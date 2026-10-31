(import (liii check) (liii json schema budget))

(check-set-mode! 'report-failed)

;; budget-max-steps
;; 读取资源预算允许的最大 Schema 节点评估数。
;;
;; 语法
;; ----
;; (budget-max-steps budget)
;;
;; 参数
;; ----
;; budget : budget
;; 由 make-resource-budget 创建的预算记录。
;;
;; 返回值
;; ----
;; integer
;; 最大评估步数。
;;
;; 注意
;; ----
;; 该上限在预算创建时确定，复位计数器不会改变它。
;;
;; 示例
;; ----
;; (budget-max-steps (make-resource-budget 10 100 1000)) => 100
;;
;; 错误处理
;; ----
;; 无。

(define budget (make-resource-budget 10 100 1000))

(check (budget-max-steps budget) => 100)
(reset-resource-budget! budget)
(check (budget-max-steps budget) => 100)

(check-report)
