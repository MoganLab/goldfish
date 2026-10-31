(import (liii check) (liii json schema budget))

(check-set-mode! 'report-failed)

;; make-resource-budget
;; 创建资源预算。
;;
;; 语法
;; ----
;; (make-resource-budget max-depth max-steps max-references)
;;
;; 参数
;; ----
;; max-depth : integer
;; 单次校验允许的最大求值深度。
;;
;; max-steps : integer
;; 单次校验允许的最大 Schema 节点评估数。
;;
;; max-references : integer
;; 单次校验允许的最大引用解析次数。
;;
;; 返回值
;; ----
;; budget
;; 预算记录，保存三个上限与三个计数器。
;;
;; 注意
;; ----
;; 计数器初始为 0，每次校验运行前会复位，上限保持不变。
;;
;; 示例
;; ----
;; (budget-max-depth (make-resource-budget 10 100 1000)) => 10
;;
;; 错误处理
;; ----
;; 无。

(define budget (make-resource-budget 10 100 1000))

(check-true (resource-budget? budget))
(check (budget-max-depth budget) => 10)
(check (budget-max-steps budget) => 100)
(check (budget-max-references budget) => 1000)
(check (budget-depth budget) => 0)
(check (budget-steps budget) => 0)
(check (budget-references budget) => 0)

(check-report)
