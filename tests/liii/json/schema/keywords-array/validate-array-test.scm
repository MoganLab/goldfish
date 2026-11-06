;; validate-array
;; 校验数组类关键字并返回错误列表。
;;
;; 语法
;; ----
;; (validate-array validator keyword value instance schema path schema-path
;;   root descend valid?)
;;
;; 参数
;; ----
;; validator : any?
;; 当前校验器，用于解析 unevaluatedItems 所需的已求值注解。
;;
;; keyword : string
;; 关键字名称，例如 "items"、"contains"。
;;
;; value : any?
;; 关键字在 Schema 中的取值。
;;
;; instance : any?
;; 待校验的实例。
;;
;; schema : any?
;; 实例所属的 Schema 对象。
;;
;; path : list
;; 实例在根资源中的路径段列表。
;;
;; schema-path : list
;; 当前 Schema 在根资源中的路径段列表。
;;
;; root : any?
;; 根资源。
;;
;; descend : procedure
;; 下钻到子 Schema 的过程，签名为
;; (descend instance schema path schema-path root) => 错误列表。
;;
;; valid? : procedure
;; 判断实例是否满足子 Schema 的过程，签名为
;; (valid? instance schema root) => boolean。
;;
;; 返回值
;; ----
;; list
;; 校验失败时返回错误列表，通过时返回空列表。
;;
;; 注意
;; ----
;; 实例不是数组时直接返回空列表，不产生错误。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema error) (liii json schema keywords-array))

(check-set-mode! 'report-failed)

;; descend 记录每次下钻的子实例、子 Schema 与路径。

(define calls '())

(define (descend child-instance child-schema path schema-path root)
  (set! calls (cons (list child-instance child-schema path schema-path) calls))
  '()
) ;define

;; valid? 把数字型子 Schema 当作期望值，用于 contains 计数。

(define (valid? child-instance child-schema root)
  (and (number? child-instance)
    (number? child-schema)
    (= child-instance child-schema)
  ) ;and
) ;define


(define (reset-calls!)
  (set! calls '())
) ;define

;; count-errors 统计单个关键字产生的错误数量。

(define (count-errors keyword value instance schema)
  (length (validate-array #f keyword value instance schema '() '() #f descend valid?)
  ) ;length
) ;define

;; 非数组实例不产生错误。

(check (count-errors "minItems" 1 "x" '(("minItems" . 1))) => 0)

;; minItems 与 maxItems。

(check (count-errors "minItems" 2 #(1 2) '(("minItems" . 2))) => 0)
(check (count-errors "minItems" 2 #(1) '(("minItems" . 2))) => 1)
(check (count-errors "maxItems" 2 #(1 2) '(("maxItems" . 2))) => 0)
(check (count-errors "maxItems" 2 #(1 2 3) '(("maxItems" . 2))) => 1)

;; uniqueItems 按 JSON 语义比较元素。

(check (count-errors "uniqueItems" #t #(1 2) '(("uniqueItems" . #t))) => 0)
(check (count-errors "uniqueItems" #t #(1 1) '(("uniqueItems" . #t))) => 1)
(check (count-errors "uniqueItems" #t #(1 1.0) '(("uniqueItems" . #t))) => 1)
(check (count-errors "uniqueItems" #f #(1 1) '(("uniqueItems" . #f))) => 0)

;; uniqueItems 区分嵌套的布尔与数字，也区分对象键的顺序。

(check (count-errors "uniqueItems" #t #(#t 1 #f 0) '(("uniqueItems" . #t)))
  =>
  0
) ;check
(check (count-errors "uniqueItems" #t #(#(1 #t)) '(("uniqueItems" . #t))) => 0)

;; contains 统计满足子 Schema 的元素个数。

(check (count-errors "contains" 1 #(1 2 3) '(("contains" . 1))) => 0)
(check (count-errors "contains" 1 #(2 3) '(("contains" . 1))) => 1)
(check (count-errors "contains" 1 #(1 2 3) '(("contains" . 1)
                                             ("minContains" . 2)))
  =>
  1
) ;check

;; prefixItems 按位置下钻，路径带上元素下标。

(define type-integer '(("type" . "integer")))

(define prefix-schema
  '(("prefixItems" . #((("type" . "integer")) (("type" . "string")))))
) ;define

(reset-calls!)
(check (count-errors "prefixItems"
         #((("type" . "integer")) (("type" . "string"))) #(1 2) prefix-schema
       ) ;count-errors
  =>
  0
) ;check
(check (length calls) => 2)
(check (car (car calls)) => 2)
(check (cadr (car calls)) => '(("type" . "string")))
(check (caddr (car calls)) => '(1))

;; items 从 prefixItems 之后的位置开始下钻。

(define items-schema
  '(("prefixItems" . #((("type" . "integer"))))
    ("items" . #((("type" . "integer")))))
) ;define

(reset-calls!)
(check (count-errors "items" type-integer #(1 2 3) items-schema) => 0)
(check (length calls) => 2)
(check (car (car calls)) => 3)
(check (caddr (car calls)) => '(2))

;; unevaluatedItems 只处理未被 prefixItems 覆盖的位置。

(define prefix-only-schema '(("prefixItems" . #((("type" . "integer"))))))

(reset-calls!)
(check (count-errors "unevaluatedItems" #f #(1 2) prefix-only-schema) => 1)
(check (length calls) => 0)

(reset-calls!)
(check (count-errors "unevaluatedItems" type-integer #(1 2) prefix-only-schema)
  =>
  0
) ;check
(check (length calls) => 1)
(check (car (car calls)) => 2)
(check (caddr (car calls)) => '(1))

;; 错误对象携带关键字与路径信息。

(define error
  (car (validate-array #f
         "maxItems"
         1
         #(1 2)
         '(("maxItems" . 1))
         '(("items" 0))
         '("root")
         #f
         descend
         valid?
       ) ;validate-array
  ) ;car
) ;define
(check (json-schema-error-keyword error) => "maxItems")
(check (json-schema-error-keyword-value error) => 1)
(check (json-schema-error-instance error) => #(1 2))
(check (json-schema-error-path error) => '(("items" 0)))
(check (json-schema-error-schema-path error) => '("root" "maxItems"))

(check-report)
