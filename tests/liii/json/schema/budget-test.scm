;; (liii json schema budget) 模块介绍与用法示例
;;
;; (liii json schema budget) 提供单次校验的资源预算：限制求值深度、
;; Schema 节点评估数与引用解析次数，避免递归 Schema 无限展开。

(import (liii json schema budget))

;; ==== 模块概述 ====
;;
;; - make-resource-budget        - 创建预算
;; - resource-budget?            - 判断是否为预算
;; - reset-resource-budget!      - 复位计数器
;; - enter-budget-depth!         - 进入一层求值深度
;; - leave-budget-depth!         - 退出一层求值深度
;; - count-budget-step!          - 累加一次节点评估
;; - count-budget-reference!     - 累加一次引用解析
;; - budget-depth                - 读取当前深度
;; - budget-steps                - 读取已评估节点数
;; - budget-references           - 读取已解析引用数
;; - budget-max-depth            - 读取最大深度
;; - budget-max-steps            - 读取最大评估数
;; - budget-max-references       - 读取最大引用数
;; - validate-budget-options!    - 校验预算选项
;; - resource-budget-from-options - 按选项创建预算

;; ==== 如何查看函数的文档和用例 ====
;;
;; 查看模块文档：
;;   gf doc liii/json/schema/budget
;;
;; 查看具体函数文档：
;;   gf doc "make-resource-budget"
;;   gf doc "budget-depth"

;; ==== 常见用法示例 ===;

;; 示例1：创建预算并读取上限

(define budget (make-resource-budget 10 100 1000))
(budget-max-depth budget)
(budget-depth budget)

;; 示例2：超限时抛出 json-schema-budget-exceeded
(catch 'json-schema-budget-exceeded
  (lambda () (enter-budget-depth! (make-resource-budget 0 100 100)))
  (lambda args 'budget-exceeded)
) ;catch

;; 示例3：校验预算选项
(catch 'json-schema-options-error
  (lambda () (validate-budget-options! '((max-depth . 0))))
  (lambda args 'bad-option)
) ;catch
