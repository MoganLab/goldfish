(import (liii check) (liii json schema dialect))

(check-set-mode! 'report-failed)

;; 返回值是 member 的结果（真值但不严格等于 #t），这里归一化为布尔。

(define (truthy? value)
  (not (eq? value #f))
) ;define

;; subschema-location?
;; 判断关键字的值是否存放子 Schema。
;;
;; 语法
;; ----
;; (subschema-location? keyword)
;;
;; 参数
;; ----
;; keyword : string
;; 关键字名称。
;;
;; 返回值
;; ----
;; boolean
;; 是子 Schema 位置返回真值，否则返回 #f；
;; 实际返回值是 member 的结果，不是严格的 #t。
;;
;; 注意
;; ----
;; 内嵌 $id、$anchor 与 $dynamicAnchor 的发现只跟随这些位置，
;; enum、const、examples、default 等位置里的 $id 不建立新的基 URI。
;;
;; 示例
;; ----
;; (subschema-location? "properties") => #t
;; (subschema-location? "enum") => #f
;;
;; 错误处理
;; ----
;; 无。

(check-true (truthy? (subschema-location? "properties")))
(check-true (truthy? (subschema-location? "patternProperties")))
(check-true (truthy? (subschema-location? "additionalProperties")))
(check-true (truthy? (subschema-location? "dependentSchemas")))
(check-true (truthy? (subschema-location? "unevaluatedProperties")))
(check-true (truthy? (subschema-location? "prefixItems")))
(check-true (truthy? (subschema-location? "items")))
(check-true (truthy? (subschema-location? "contains")))
(check-true (truthy? (subschema-location? "unevaluatedItems")))
(check-true (truthy? (subschema-location? "allOf")))
(check-true (truthy? (subschema-location? "anyOf")))
(check-true (truthy? (subschema-location? "oneOf")))
(check-true (truthy? (subschema-location? "not")))
(check-true (truthy? (subschema-location? "if")))
(check-true (truthy? (subschema-location? "then")))
(check-true (truthy? (subschema-location? "else")))
(check-true (truthy? (subschema-location? "$defs")))

(check-false (truthy? (subschema-location? "enum")))
(check-false (truthy? (subschema-location? "const")))
(check-false (truthy? (subschema-location? "type")))
(check-false (truthy? (subschema-location? "default")))

(check-report)
