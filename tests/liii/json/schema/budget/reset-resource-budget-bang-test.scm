(import (liii check) (liii json schema budget))

(check-set-mode! 'report-failed)

;; reset-resource-budget!
;; 复位资源预算的计数器，保留上限。
;;
;; 语法
;; ----
;; (reset-resource-budget! budget)
;;
;; 参数
;; ----
;; budget : budget
;; 由 make-resource-budget 创建的预算记录。
;;
;; 返回值
;; ----
;; budget
;; 复位后的预算记录。
;;
;; 注意
;; ----
;; 深度、步数与引用三个计数器都被清零，上限保持不变；
;; 复位后预算可以重新接受完整额度。
;;
;; 示例
;; ----
;; (reset-resource-budget! budget)
;;
;; 错误处理
;; ----
;; 无。

(define budget (make-resource-budget 2 100 100))

(enter-budget-depth! budget)
(count-budget-step! budget)
(count-budget-reference! budget)
(check (budget-depth budget) => 1)
(check (budget-steps budget) => 1)
(check (budget-references budget) => 1)

(reset-resource-budget! budget)
(check (budget-depth budget) => 0)
(check (budget-steps budget) => 0)
(check (budget-references budget) => 0)

;; 上限保持不变
(check (budget-max-depth budget) => 2)
(check (budget-max-steps budget) => 100)
(check (budget-max-references budget) => 100)

;; 复位后可以重新使用完整额度
(enter-budget-depth! budget)
(check (budget-depth budget) => 1)
(count-budget-step! budget)
(check (budget-steps budget) => 1)

(check-report)
