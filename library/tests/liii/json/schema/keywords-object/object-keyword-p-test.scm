;; object-keyword?
;; 判断关键字是否属于对象类关键字。
;;
;; 语法
;; ----
;; (object-keyword? keyword)
;;
;; 参数
;; ----
;; keyword : string
;; 关键字名称，例如 "properties"、"required"。
;;
;; 返回值
;; ----
;; boolean
;; 属于对象类关键字返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 对象类关键字包括 properties、required、additionalProperties、propertyNames、
;; minProperties、maxProperties、dependentRequired、dependentSchemas、
;; unevaluatedProperties、patternProperties。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema keywords-object))

(check-set-mode! 'report-failed)

;; member 返回的是匹配位置的尾部（真值但不严格等于 #t），这里归一化为布尔。

(define (truthy? value)
  (not (eq? value #f))
) ;define

(check-true (truthy? (object-keyword? "properties")))
(check-true (truthy? (object-keyword? "required")))
(check-true (truthy? (object-keyword? "additionalProperties")))
(check-true (truthy? (object-keyword? "propertyNames")))
(check-true (truthy? (object-keyword? "minProperties")))
(check-true (truthy? (object-keyword? "maxProperties")))
(check-true (truthy? (object-keyword? "dependentRequired")))
(check-true (truthy? (object-keyword? "dependentSchemas")))
(check-true (truthy? (object-keyword? "unevaluatedProperties")))
(check-true (truthy? (object-keyword? "patternProperties")))

(check-false (truthy? (object-keyword? "items")))
(check-false (truthy? (object-keyword? "type")))
(check-false (truthy? (object-keyword? "")))

(check-report)
