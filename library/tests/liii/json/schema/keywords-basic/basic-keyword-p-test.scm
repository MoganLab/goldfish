;; basic-keyword?
;; 判断关键字是否属于基础断言类关键字。
;;
;; 语法
;; ----
;; (basic-keyword? keyword)
;;
;; 参数
;; ----
;; keyword : string
;; 关键字名称，例如 "type"、"minimum"。
;;
;; 返回值
;; ----
;; boolean
;; 属于基础断言类关键字返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 基础断言类关键字包括 type、const、enum、minimum、maximum、
;; exclusiveMinimum、exclusiveMaximum、multipleOf、minLength、maxLength。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema keywords-basic))

(check-set-mode! 'report-failed)

;; member 返回的是匹配位置的尾部（真值但不严格等于 #t），这里归一化为布尔。

(define (truthy? value)
  (not (eq? value #f))
) ;define

(check-true (truthy? (basic-keyword? "type")))
(check-true (truthy? (basic-keyword? "const")))
(check-true (truthy? (basic-keyword? "enum")))
(check-true (truthy? (basic-keyword? "minimum")))
(check-true (truthy? (basic-keyword? "maximum")))
(check-true (truthy? (basic-keyword? "exclusiveMinimum")))
(check-true (truthy? (basic-keyword? "exclusiveMaximum")))
(check-true (truthy? (basic-keyword? "multipleOf")))
(check-true (truthy? (basic-keyword? "minLength")))
(check-true (truthy? (basic-keyword? "maxLength")))

(check-false (truthy? (basic-keyword? "properties")))
(check-false (truthy? (basic-keyword? "items")))
(check-false (truthy? (basic-keyword? "allOf")))
(check-false (truthy? (basic-keyword? "")))

(check-report)
