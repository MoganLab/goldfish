(import (liii check) (liii json schema budget))

(check-set-mode! 'report-failed)

;; budget-depth
;; 读取资源预算当前的求值深度。
;;
;; 语法
;; ----
;; (budget-depth budget)
;;
;; 参数
;; ----
;; budget : budget
;; 由 make-resource-budget 创建的预算记录。
;;
;; 返回值
;; ----
;; integer
;; 当前求值深度。
;;
;; 注意
;; ----
;; 深度由 enter-budget-depth! 增加、leave-budget-depth! 减少。
;;
;; 示例
;; ----
;; (budget-depth (make-resource-budget 10 100 1000)) => 0
;;
;; 错误处理
;; ----
;; 无。

(define budget (make-resource-budget 10 100 1000))

(check (budget-depth budget) => 0)
(enter-budget-depth! budget)
(check (budget-depth budget) => 1)
(enter-budget-depth! budget)
(check (budget-depth budget) => 2)
(leave-budget-depth! budget)
(check (budget-depth budget) => 1)

(check-report)
