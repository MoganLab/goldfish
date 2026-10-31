(import (liii check) (liii json schema budget))

(check-set-mode! 'report-failed)

;; budget-references
;; 读取资源预算当前已解析的引用次数。
;;
;; 语法
;; ----
;; (budget-references budget)
;;
;; 参数
;; ----
;; budget : budget
;; 由 make-resource-budget 创建的预算记录。
;;
;; 返回值
;; ----
;; integer
;; 已解析的引用次数。
;;
;; 注意
;; ----
;; 次数由 count-budget-reference! 累加，每次校验运行前复位。
;;
;; 示例
;; ----
;; (budget-references (make-resource-budget 10 100 1000)) => 0
;;
;; 错误处理
;; ----
;; 无。

(define budget (make-resource-budget 10 100 1000))

(check (budget-references budget) => 0)
(count-budget-reference! budget)
(check (budget-references budget) => 1)
(count-budget-reference! budget)
(check (budget-references budget) => 2)

(check-report)
