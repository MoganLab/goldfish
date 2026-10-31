;; (liii json schema meta) 模块介绍与用法示例
;;
;; (liii json schema meta) 内嵌 JSON Schema 2020-12 元 Schema 及其各词汇表，
;; 使 check-schema 可以在完全离线的环境中校验 Schema 本身。

(import (liii json schema meta))

;; ==== 模块概述 ====
;;
;; - json-schema-meta-uris     - 内嵌元 Schema 资源的 URI 列表
;; - json-schema-meta-resource - 按 URI 读取内嵌元 Schema 资源
;;
;; 内嵌资源包括主元 Schema 与 core、applicator、validation、unevaluated、
;; format-annotation、format-assertion、content、meta-data 八个词汇表。

;; ==== 如何查看函数的文档和用例 ====
;;
;; 查看模块文档：
;;   gf doc liii/json/schema/meta
;;
;; 查看具体函数文档：
;;   gf doc "json-schema-meta-resource"

;; ==== 常见用法示例 ===;

;; 示例1：列出内嵌资源
(length (json-schema-meta-uris)) ; => 9

;; 示例2：读取主元 Schema
(json-schema-meta-resource "https://json-schema.org/draft/2020-12/schema")
