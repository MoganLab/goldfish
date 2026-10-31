(import (liii check) (liii json schema budget))

(check-set-mode! 'report-failed)

;; budget-steps
;; 读取资源预算当前已评估的 Schema 节点数。
;;
;; 语法
;; ----
;; (budget-steps budget)
;;
;; 参数
;; ----
;; budget : budget
;; 由 make-resource-budget 创建的预算记录。
;;
;; 返回值
;; ----
;; integer
;; 已评估的 Schema 节点数。
;;
;; 注意
;; ----
;; 步数由 count-budget-step! 累加，每次校验运行前复位。
;;
;; 示例
;; ----
;; (budget-steps (make-resource-budget 10 100 1000)) => 0
;;
;; 错误处理
;; ----
;; 无。

(define budget (make-resource-budget 10 100 1000))

(check (budget-steps budget) => 0)
(count-budget-step! budget)
(check (budget-steps budget) => 1)
(count-budget-step! budget)
(check (budget-steps budget) => 2)

(check-report)
