(import (liii check) (liii json schema budget))

(check-set-mode! 'report-failed)

;; resource-budget-from-options
;; 按选项表创建资源预算。
;;
;; 语法
;; ----
;; (resource-budget-from-options options)
;;
;; 参数
;; ----
;; options : alist
;; 选项表，可包含 max-depth、max-steps 与 max-references。
;;
;; 返回值
;; ----
;; budget
;; 资源预算记录。
;;
;; 注意
;; ----
;; 缺省上限为深度 1000、步数 1000000、引用 100000；
;; 该函数不校验选项取值，校验请使用 validate-budget-options!。
;;
;; 示例
;; ----
;; (budget-max-depth (resource-budget-from-options '((max-depth . 10)))) => 10
;;
;; 错误处理
;; ----
;; 无。

(define budget
  (resource-budget-from-options '((max-depth . 10)
                                  (max-steps . 100)
                                  (max-references . 1000))
  ) ;resource-budget-from-options
) ;define

(check-true (resource-budget? budget))
(check (budget-max-depth budget) => 10)
(check (budget-max-steps budget) => 100)
(check (budget-max-references budget) => 1000)
(check (budget-depth budget) => 0)

;; 缺省上限

(define defaults (resource-budget-from-options '()))
(check (budget-max-depth defaults) => 1000)
(check (budget-max-steps defaults) => 1000000)
(check (budget-max-references defaults) => 100000)

(check-report)
