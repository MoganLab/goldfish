(import (liii check) (liii json schema budget))

(check-set-mode! 'report-failed)

;; budget-max-references
;; 读取资源预算允许的最大引用解析次数。
;;
;; 语法
;; ----
;; (budget-max-references budget)
;;
;; 参数
;; ----
;; budget : budget
;; 由 make-resource-budget 创建的预算记录。
;;
;; 返回值
;; ----
;; integer
;; 最大引用解析次数。
;;
;; 注意
;; ----
;; 该上限在预算创建时确定，复位计数器不会改变它。
;;
;; 示例
;; ----
;; (budget-max-references (make-resource-budget 10 100 1000)) => 1000
;;
;; 错误处理
;; ----
;; 无。

(define budget (make-resource-budget 10 100 1000))

(check (budget-max-references budget) => 1000)
(reset-resource-budget! budget)
(check (budget-max-references budget) => 1000)

(check-report)
