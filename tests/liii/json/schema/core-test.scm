;; (liii json schema core) 模块介绍与用法示例
;;
;; (liii json schema core) 提供单节点求值入口：在组合关键字内部继续
;; 下钻到子 Schema 时，用它携带当前的实例路径与 Schema 路径。

(import (liii json schema) (liii json schema core))

;; ==== 模块概述 ====
;;
;; - json-schema-node-valid?   - 以指定根资源校验单个节点，返回布尔结果
;; - json-schema-descend-errors - 以指定根资源校验单个节点，返回错误列表
;;
;; 节点的遍历逻辑由 (liii json schema evaluator) 实现，
;; 关键字语义由 (liii json schema keywords) 及其子模块实现。

;; ==== 如何查看函数的文档和用例 ====
;;
;; 查看模块文档：
;;   gf doc liii/json/schema/core
;;
;; 查看具体函数文档：
;;   gf doc "json-schema-node-valid?"
;;   gf doc "json-schema-descend-errors"

;; ==== 常见用法示例 ====

(define validator (make-json-schema-validator #t))

;; 示例1：判断单个节点是否合法
(json-schema-node-valid? validator 1 '(("type" . "integer")) #t)

;; 示例2：下钻到子 Schema 并收集错误
(json-schema-descend-errors validator
  "x"
  '(("type" . "integer"))
  '("rows" 0)
  '("items")
  #t
) ;json-schema-descend-errors
