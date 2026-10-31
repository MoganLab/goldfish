(import (liii check) (liii json schema dialect))

(check-set-mode! 'report-failed)

;; 返回值是 member 的结果（真值但不严格等于 #t），这里归一化为布尔。
(define (truthy? value)
  (not (eq? value #f))
) ;define

;; schema-map-keyword?
;; 判断关键字的值是否是“名称到 Schema”的映射。
;;
;; 语法
;; ----
;; (schema-map-keyword? keyword)
;;
;; 参数
;; ----
;; keyword : string
;; 关键字名称。
;;
;; 返回值
;; ----
;; boolean
;; 是映射关键字返回真值，否则返回 #f；
;; 实际返回值是 member 的结果，不是严格的 #t。
;;
;; 注意
;; ----
;; 映射自身的键是属性名，不是 Schema 关键字。
;;
;; 示例
;; ----
;; (schema-map-keyword? "properties") => #t
;; (schema-map-keyword? "items") => #f
;;
;; 错误处理
;; ----
;; 无。

(check-true (truthy? (schema-map-keyword? "properties")))
(check-true (truthy? (schema-map-keyword? "patternProperties")))
(check-true (truthy? (schema-map-keyword? "dependentSchemas")))
(check-true (truthy? (schema-map-keyword? "$defs")))

(check-false (truthy? (schema-map-keyword? "items")))
(check-false (truthy? (schema-map-keyword? "required")))
(check-false (truthy? (schema-map-keyword? "type")))

(check-report)
