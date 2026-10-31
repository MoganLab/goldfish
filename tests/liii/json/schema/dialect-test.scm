;; (liii json schema dialect) 模块介绍与用法示例
;;
;; (liii json schema dialect) 负责方言识别与关键字表：确认 Schema 使用的
;; 草稿版本，并判断关键字的位置类别。

(import (liii json schema dialect) (liii json schema) (liii check))

(check-set-mode! 'report-failed)

;; ==== 模块概述 ====
;;
;; - draft-for                    - 返回 Schema 使用的方言
;; - known-keyword?               - 是否为已知关键字
;; - unsupported-modern-keyword!  - 检查尚未支持的新版关键字
;; - subschema-location?          - 关键字的值是否存放子 Schema
;; - schema-map-keyword?          - 关键字的值是否是名称到 Schema 的映射
;;
;; 目前只支持 2020-12 方言，其他草稿声明会报 json-schema-unknown-dialect。

;; ==== 如何查看函数的文档和用例 ====
;;
;; 查看模块文档：
;;   gf doc liii/json/schema/dialect
;;
;; 查看具体函数文档：
;;   gf doc "draft-for"

;; ==== 常见用法示例 ===;

;; 示例1：未声明 $schema 时默认 2020-12
(draft-for '(("type" . "integer"))) ; => 'draft2020-12

;; 示例2：其他草稿被拒绝
(catch 'json-schema-unknown-dialect
  (lambda () (draft-for '(("$schema" . "http://json-schema.org/draft-07/schema#"))))
  (lambda args 'unknown-dialect))
; => 'unknown-dialect

;; ==== 暂存的集成断言 ====

(check-catch 'json-schema-options-error
  (json-schema-valid? 1 #t '((draft . draft2020-12)))
) ;check-catch

(check-report)
