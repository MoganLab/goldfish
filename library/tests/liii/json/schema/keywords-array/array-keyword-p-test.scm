;; array-keyword?
;; 判断关键字是否属于数组类关键字。
;;
;; 语法
;; ----
;; (array-keyword? keyword)
;;
;; 参数
;; ----
;; keyword : string
;; 关键字名称，例如 "items"、"contains"。
;;
;; 返回值
;; ----
;; boolean
;; 属于数组类关键字返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 数组类关键字包括 prefixItems、items、minItems、maxItems、
;; uniqueItems、contains、unevaluatedItems。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema keywords-array))

(check-set-mode! 'report-failed)

;; member 返回的是匹配位置的尾部（真值但不严格等于 #t），这里归一化为布尔。

(define (truthy? value)
  (not (eq? value #f))
) ;define

(check-true (truthy? (array-keyword? "prefixItems")))
(check-true (truthy? (array-keyword? "items")))
(check-true (truthy? (array-keyword? "minItems")))
(check-true (truthy? (array-keyword? "maxItems")))
(check-true (truthy? (array-keyword? "uniqueItems")))
(check-true (truthy? (array-keyword? "contains")))
(check-true (truthy? (array-keyword? "unevaluatedItems")))

(check-false (truthy? (array-keyword? "properties")))
(check-false (truthy? (array-keyword? "type")))
(check-false (truthy? (array-keyword? "")))

(check-report)
