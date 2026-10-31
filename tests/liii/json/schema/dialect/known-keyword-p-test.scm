(import (liii check) (liii json schema dialect))

(check-set-mode! 'report-failed)

;; 返回值是 member 的结果（真值但不严格等于 #t），这里归一化为布尔。

(define (truthy? value)
  (not (eq? value #f))
) ;define

;; known-keyword?
;; 判断关键字是否为 2020-12 的已知关键字。
;;
;; 语法
;; ----
;; (known-keyword? keyword)
;;
;; 参数
;; ----
;; keyword : string
;; 关键字名称。
;;
;; 返回值
;; ----
;; boolean
;; 是已知关键字返回真值，否则返回 #f；
;; 实际返回值是 member 的结果，不是严格的 #t。
;;
;; 注意
;; ----
;; 未知关键字会被忽略，但注解类关键字（如 title）不参与校验。
;;
;; 示例
;; ----
;; (known-keyword? "type") => #t
;; (known-keyword? "x-unknown") => #f
;;
;; 错误处理
;; ----
;; 无。

(check-true (truthy? (known-keyword? "type")))
(check-true (truthy? (known-keyword? "const")))
(check-true (truthy? (known-keyword? "enum")))
(check-true (truthy? (known-keyword? "properties")))
(check-true (truthy? (known-keyword? "required")))
(check-true (truthy? (known-keyword? "additionalProperties")))
(check-true (truthy? (known-keyword? "propertyNames")))
(check-true (truthy? (known-keyword? "minProperties")))
(check-true (truthy? (known-keyword? "maxProperties")))
(check-true (truthy? (known-keyword? "dependentRequired")))
(check-true (truthy? (known-keyword? "dependentSchemas")))
(check-true (truthy? (known-keyword? "unevaluatedProperties")))
(check-true (truthy? (known-keyword? "prefixItems")))
(check-true (truthy? (known-keyword? "items")))
(check-true (truthy? (known-keyword? "minItems")))
(check-true (truthy? (known-keyword? "maxItems")))
(check-true (truthy? (known-keyword? "uniqueItems")))
(check-true (truthy? (known-keyword? "contains")))
(check-true (truthy? (known-keyword? "unevaluatedItems")))
(check-true (truthy? (known-keyword? "minimum")))
(check-true (truthy? (known-keyword? "maximum")))
(check-true (truthy? (known-keyword? "exclusiveMinimum")))
(check-true (truthy? (known-keyword? "exclusiveMaximum")))
(check-true (truthy? (known-keyword? "multipleOf")))
(check-true (truthy? (known-keyword? "minLength")))
(check-true (truthy? (known-keyword? "maxLength")))
(check-true (truthy? (known-keyword? "pattern")))
(check-true (truthy? (known-keyword? "patternProperties")))
(check-true (truthy? (known-keyword? "allOf")))
(check-true (truthy? (known-keyword? "anyOf")))
(check-true (truthy? (known-keyword? "oneOf")))
(check-true (truthy? (known-keyword? "not")))
(check-true (truthy? (known-keyword? "if")))
(check-true (truthy? (known-keyword? "$ref")))
(check-true (truthy? (known-keyword? "$dynamicRef")))
(check-true (truthy? (known-keyword? "format")))

(check-false (truthy? (known-keyword? "x-unknown")))
(check-false (truthy? (known-keyword? "title")))
(check-false (truthy? (known-keyword? "$id")))
(check-false (truthy? (known-keyword? "")))

(check-report)
