(import (liii check) (liii json schema budget))

(check-set-mode! 'report-failed)

;; leave-budget-depth!
;; 退出一层求值深度并返回新的深度。
;;
;; 语法
;; ----
;; (leave-budget-depth! budget)
;;
;; 参数
;; ----
;; budget : budget
;; 由 make-resource-budget 创建的预算记录。
;;
;; 返回值
;; ----
;; integer
;; 退出后的求值深度。
;;
;; 注意
;; ----
;; 深度不会小于 0，多余的退出调用会被忽略。
;;
;; 示例
;; ----
;; (leave-budget-depth! budget) => 0
;;
;; 错误处理
;; ----
;; 无。

(define budget (make-resource-budget 10 100 1000))

(enter-budget-depth! budget)
(enter-budget-depth! budget)
(check (budget-depth budget) => 2)

(check (leave-budget-depth! budget) => 1)
(check (budget-depth budget) => 1)
(check (leave-budget-depth! budget) => 0)
(check (budget-depth budget) => 0)

;; 深度不会小于 0
(leave-budget-depth! budget)
(check (budget-depth budget) => 0)

(check-report)
