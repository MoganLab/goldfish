;; evaluated-items
;; 收集数组实例中已被求值的元素下标。
;;
;; 语法
;; ----
;; (evaluated-items validator instance schema root valid?)
;;
;; 参数
;; ----
;; validator : any?
;; 当前校验器，用于解析 $ref 与 $dynamicRef。
;;
;; instance : any?
;; 待校验的实例。
;;
;; schema : any?
;; 实例所属的 Schema 对象。
;;
;; root : any?
;; 根资源。
;;
;; valid? : procedure
;; 判断实例是否满足子 Schema 的过程，签名为
;; (valid? instance schema root) => boolean。
;;
;; 返回值
;; ----
;; list
;; 已求值的元素下标列表。
;;
;; 注意
;; ----
;; 实例不是数组或 Schema 不是 JSON 对象时返回空列表。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema evaluated) (liii sort))

(check-set-mode! 'report-failed)

;; valid? 恒为真，表示每个元素都匹配 contains 子 Schema。

(define (always-valid? instance schema root)
  #t
) ;define

;; valid? 只接受数字类型的实例。

(define (number-valid? instance schema root)
  (number? instance)
) ;define



;; 测试用到的 Schema 片段。

(define type-integer '(("type" . "integer")))

(define subschema-vector (vector type-integer))

(define schema-prefix-items (list (cons "prefixItems" subschema-vector)))

(define schema-items (list (cons "items" subschema-vector)))

(define schema-contains (list (cons "contains" type-integer)))

(define schema-allOf-items (list (cons "allOf" (vector schema-items))))

(define schema-anyOf-prefix (list (cons "anyOf" (vector schema-prefix-items))))

(define schema-oneOf-prefix (list (cons "oneOf" (vector schema-prefix-items))))

;; prefixItems 覆盖前缀位置。

(check (list-sort < (evaluated-items #f #(1 2 3) schema-prefix-items #f always-valid?))
  =>
  '(0)
) ;check

;; items 覆盖 prefixItems 之后的所有位置。

(check
  (list-sort <
    (evaluated-items #f
      #(1 2 3)
      (list (cons "prefixItems" subschema-vector) (cons "items" subschema-vector))
      #f
      always-valid?
    ) ;evaluated-items
  ) ;list-sort
  =>
  '(0 1 2)
) ;check

;; contains 只覆盖匹配的元素。

(check (list-sort < (evaluated-items #f #(1 "x" 3) schema-contains #f number-valid?))
  =>
  '(0 2)
) ;check

;; allOf 分支匹配时合并分支中已求值的下标。

(check (list-sort < (evaluated-items #f #(1 2 3) schema-allOf-items #f always-valid?))
  =>
  '(0 1 2)
) ;check

;; anyOf 分支匹配时合并匹配分支中已求值的下标。

(check (list-sort < (evaluated-items #f #(1 2 3) schema-anyOf-prefix #f always-valid?))
  =>
  '(0)
) ;check

;; oneOf 分支只有一个匹配时才合并。

(check (list-sort < (evaluated-items #f #(1 2 3) schema-oneOf-prefix #f always-valid?))
  =>
  '(0)
) ;check

;; 实例不是数组或 Schema 不是对象时返回空列表。

(check (evaluated-items #f "x" schema-items #f always-valid?) => '())
(check (evaluated-items #f #(1) #t #f always-valid?) => '())

(check-report)
