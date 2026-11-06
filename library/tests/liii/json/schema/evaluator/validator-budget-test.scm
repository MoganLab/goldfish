;; validator-budget
;; 从校验器选项中读取资源预算记录。
;;
;; 语法
;; ----
;; (validator-budget validator)
;;
;; 参数
;; ----
;; validator : validator
;; 由 make-json-schema-validator 创建的校验器。
;;
;; 返回值
;; ----
;; resource-budget?
;; 预算记录；make-json-schema-validator 会按 max-depth、max-steps、
;; max-references 选项自动创建预算并放入选项表。
;;
;; 说明
;; ----
;; 预算随校验器选项传递，validate-node 与引用解析在每次校验中
;; 通过该记录统计深度、步数与引用次数。

(import (liii check)
  (liii json schema)
  (liii json schema budget)
  (liii json schema evaluator)
) ;import

(check-set-mode! 'report-failed)

(define validator
  (make-json-schema-validator '(("type" . "integer"))
    '((check-schema? . #f)
      (max-depth . 7)
      (max-steps . 70)
      (max-references . 700))
  ) ;make-json-schema-validator
) ;define

(define budget (validator-budget validator))

(check (resource-budget? budget) => #t)
(check (budget-max-depth budget) => 7)
(check (budget-max-steps budget) => 70)
(check (budget-max-references budget) => 700)

;; 同一校验器多次读取返回同一个预算记录
(check (eq? (validator-budget validator) budget) => #t)

(check-report)
