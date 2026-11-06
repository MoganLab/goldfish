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

;; 非数组实例不产生错误。

(check
  (length (validate-array #f
            "minItems"
            1
            "x"
            '(("minItems" . 1))
            '()
            '()
            #f
            descend
            valid?
          ) ;validate-array
  ) ;length
  =>
  0
) ;check

;; minItems 与 maxItems。

(check
  (length (validate-array #f
            "minItems"
            2
            #(1 2)
            '(("minItems" . 2))
            '()
            '()
            #f
            descend
            valid?
          ) ;validate-array
  ) ;length
  =>
  0
) ;check
(check
  (length (validate-array #f
            "minItems"
            2
            #(1)
            '(("minItems" . 2))
            '()
            '()
            #f
            descend
            valid?
          ) ;validate-array
  ) ;length
  =>
  1
) ;check
(check
  (length (validate-array #f
            "maxItems"
            2
            #(1 2)
            '(("maxItems" . 2))
            '()
            '()
            #f
            descend
            valid?
          ) ;validate-array
  ) ;length
  =>
  0
) ;check
(check
  (length (validate-array #f
            "maxItems"
            2
            #(1 2 3)
            '(("maxItems" . 2))
            '()
            '()
            #f
            descend
            valid?
          ) ;validate-array
  ) ;length
  =>
  1
) ;check

;; uniqueItems 按 JSON 语义比较元素。

(check
  (length (validate-array #f
            "uniqueItems"
            #t
            #(1 2)
            '(("uniqueItems" . #t))
            '()
            '()
            #f
            descend
            valid?
          ) ;validate-array
  ) ;length
  =>
  0
) ;check
(check
  (length (validate-array #f
            "uniqueItems"
            #t
            #(1 1)
            '(("uniqueItems" . #t))
            '()
            '()
            #f
            descend
            valid?
          ) ;validate-array
  ) ;length
  =>
  1
) ;check
(check
  (length (validate-array #f
            "uniqueItems"
            #t
            #(1 1.0)
            '(("uniqueItems" . #t))
            '()
            '()
            #f
            descend
            valid?
          ) ;validate-array
  ) ;length
  =>
  1
) ;check
(check
  (length (validate-array #f
            "uniqueItems"
            #f
            #(1 1)
            '(("uniqueItems" . #f))
            '()
            '()
            #f
            descend
            valid?
          ) ;validate-array
  ) ;length
  =>
  0
) ;check

;; contains 统计满足子 Schema 的元素个数。

(check
  (length (validate-array #f
            "contains"
            1
            #(1 2 3)
            '(("contains" . 1))
            '()
            '()
            #f
            descend
            valid?
          ) ;validate-array
  ) ;length
  =>
  0
) ;check
(check
  (length (validate-array #f
            "contains"
            1
            #(2 3)
            '(("contains" . 1))
            '()
            '()
            #f
            descend
            valid?
          ) ;validate-array
  ) ;length
  =>
  1
) ;check
(check
  (length (validate-array #f
            "contains"
            1
            #(1 2 3)
            '(("contains" . 1) ("minContains" . 2))
            '()
            '()
            #f
            descend
            valid?
          ) ;validate-array
  ) ;length
  =>
  1
) ;check

;; prefixItems 按位置下钻，路径带上元素下标。

(reset-calls!)
(check
  (length (validate-array #f
            "prefixItems"
            #((("type" . "integer")) (("type" . "string")))
            #(1 2)
            '(("prefixItems" . #((("type" . "integer")) (("type" . "string")))))
            '()
            '()
            #f
            descend
            valid?
          ) ;validate-array
  ) ;length
  =>
  0
) ;check
(check (length calls) => 2)
(check (car (car calls)) => 2)
(check (cadr (car calls)) => '(("type" . "string")))
(check (caddr (car calls)) => '(1))

;; items 从 prefixItems 之后的位置开始下钻。

(reset-calls!)
(check
  (length (validate-array #f
            "items"
            '(("type" . "integer"))
            #(1 2 3)
            '(("prefixItems" . #(("type" . "integer")))
              ("items" . #(("type" . "integer"))))
            '()
            '()
            #f
            descend
            valid?
          ) ;validate-array
  ) ;length
  =>
  0
) ;check
(check (length calls) => 2)
(check (car (car calls)) => 3)
(check (caddr (car calls)) => '(2))

;; unevaluatedItems 只处理未被 prefixItems 覆盖的位置。

(reset-calls!)
(check
  (length (validate-array #f
            "unevaluatedItems"
            #f
            #(1 2)
            '(("prefixItems" . #(("type" . "integer"))))
            '()
            '()
            #f
            descend
            valid?
          ) ;validate-array
  ) ;length
  =>
  1
) ;check
(check (length calls) => 0)

(reset-calls!)
(check
  (length (validate-array #f
            "unevaluatedItems"
            '(("type" . "integer"))
            #(1 2)
            '(("prefixItems" . #(("type" . "integer"))))
            '()
            '()
            #f
            descend
            valid?
          ) ;validate-array
  ) ;length
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
