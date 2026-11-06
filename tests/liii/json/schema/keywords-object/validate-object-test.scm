;; validate-object
;; 校验对象类关键字并返回错误列表。
;;
;; 语法
;; ----
;; (validate-object validator keyword value instance schema path schema-path
;;   root descend valid?)
;;
;; 参数
;; ----
;; validator : any?
;; 当前校验器，用于解析 unevaluatedProperties 所需的已求值注解。
;;
;; keyword : string
;; 关键字名称，例如 "properties"、"required"。
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
;; 实例不是 JSON 对象时直接返回空列表，不产生错误。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check)
  (liii json schema error)
  (liii json schema keywords-object)
) ;import

(check-set-mode! 'report-failed)

;; descend 记录每次下钻的子实例、子 Schema 与路径。

(define calls '())

(define (descend child-instance child-schema path schema-path root)
  (set! calls (cons (list child-instance child-schema path schema-path) calls))
  '()
) ;define

(define (valid? child-instance child-schema root)
  #t
) ;define

(define (reset-calls!)
  (set! calls '())
) ;define

;; count-errors 统计单个关键字产生的错误数量。

(define (count-errors keyword value instance schema)
  (length (validate-object #f keyword value instance schema '() '() #f descend valid?)
  ) ;length
) ;define

;; 测试用到的 Schema 片段。

(define type-integer '(("type" . "integer")))

(define property-a (cons "a" type-integer))

(define properties-a (list property-a))

(define schema-properties-a (list (cons "properties" properties-a)))

;; 非对象实例不产生错误。

(check (count-errors "required" #("a") "x" '(("required" . #("a")))) => 0)

;; properties 只下钻实例中实际出现的属性。

(reset-calls!)
(check (count-errors "properties" properties-a '(("a" . 1)) schema-properties-a)
  =>
  0
) ;check
(check (length calls) => 1)
(check (car (car calls)) => 1)
(check (cadr (car calls)) => '(("type" . "integer")))
(check (caddr (car calls)) => '("a"))

;; required 缺少属性时报错。

(check (count-errors "required" #("a" "b") '(("a" . 1)) '(("required"
                                                           . #("a" "b"))))
  =>
  1
) ;check
(check (count-errors "required" #("a") '(("a" . 1)) '(("required" . #("a"))))
  =>
  0
) ;check

;; additionalProperties 为 #f 时拒绝多余属性。

(check (count-errors "additionalProperties"
         #f
         '(("a" . 1) ("b" . 2))
         schema-properties-a
       ) ;count-errors
  =>
  1
) ;check
(check (count-errors "additionalProperties" #f '(("a" . 1)) schema-properties-a)
  =>
  0
) ;check

;; additionalProperties 为子 Schema 时下钻多余属性。

(reset-calls!)
(check (count-errors "additionalProperties"
         type-integer
         '(("a" . 1) ("b" . 2))
         schema-properties-a
       ) ;count-errors
  =>
  0
) ;check
(check (length calls) => 1)
(check (car (car calls)) => 2)
(check (caddr (car calls)) => '("b"))

;; minProperties 与 maxProperties。

(check (count-errors "minProperties" 2 '(("a" . 1) ("b" . 2)) '(("minProperties"
                                                                 . 2)))
  =>
  0
) ;check
(check (count-errors "minProperties" 2 '(("a" . 1)) '(("minProperties" . 2)))
  =>
  1
) ;check
(check (count-errors "maxProperties" 1 '(("a" . 1) ("b" . 2)) '(("maxProperties"
                                                                 . 1)))
  =>
  1
) ;check

;; dependentRequired 在依赖属性存在时要求配套属性。

(define schema-dependent-required '(("dependentRequired" ("a" . #("b")))))

(check (count-errors "dependentRequired"
         '(("a" . #("b")))
         '(("a" . 1) ("b" . 2))
         schema-dependent-required
       ) ;count-errors
  =>
  0
) ;check
(check (count-errors "dependentRequired"
         '(("a" . #("b")))
         '(("a" . 1))
         schema-dependent-required
       ) ;count-errors
  =>
  1
) ;check
(check (count-errors "dependentRequired"
         '(("a" . #("b")))
         '(("b" . 2))
         schema-dependent-required
       ) ;count-errors
  =>
  0
) ;check

;; dependentSchemas 在依赖属性存在时下钻整个实例。

(define schema-dependent-schemas
  '(("dependentSchemas" ("a" ("required" . #("b")))))
) ;define

(reset-calls!)
(check
  (count-errors "dependentSchemas"
    '(("a" ("required" . #("b"))))
    '(("a" . 1) ("b" . 2))
    schema-dependent-schemas
  ) ;count-errors
  =>
  0
) ;check
(check (length calls) => 1)
(check (car (car calls)) => '(("a" . 1) ("b" . 2)))
(check (caddr (car calls)) => '())

;; unevaluatedProperties 只处理未被 properties 覆盖的属性。

(reset-calls!)
(check (count-errors "unevaluatedProperties"
         #f
         '(("a" . 1) ("b" . 2))
         schema-properties-a
       ) ;count-errors
  =>
  1
) ;check
(check (length calls) => 0)

(reset-calls!)
(check (count-errors "unevaluatedProperties"
         type-integer
         '(("a" . 1) ("b" . 2))
         schema-properties-a
       ) ;count-errors
  =>
  0
) ;check
(check (length calls) => 1)
(check (car (car calls)) => 2)
(check (caddr (car calls)) => '("b"))

;; 错误对象携带关键字与路径信息。

(define error
  (car (validate-object #f
         "required"
         #("a")
         '(("b" . 1))
         '(("required" . #("a")))
         '(("items" 0))
         '("root")
         #f
         descend
         valid?
       ) ;validate-object
  ) ;car
) ;define
(check (json-schema-error-keyword error) => "required")
(check (json-schema-error-instance error) => '(("b" . 1)))
(check (json-schema-error-path error) => '(("items" 0)))
(check (json-schema-error-schema-path error) => '("root" "required"))

(check-report)
