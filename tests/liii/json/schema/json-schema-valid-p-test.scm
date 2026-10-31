(import (liii check) (liii json schema) (liii json))

(check-set-mode! 'report-failed)

;; json-schema-valid?
;; 判断实例是否满足 JSON Schema。
;;
;; 语法
;; ----
;; (json-schema-valid? instance schema . options)
;;
;; 参数
;; ----
;; instance : any?
;; 待校验的 JSON 数据。
;;
;; schema : any?
;; JSON Schema，可以是对象或布尔值。
;;
;; options : alist?
;; 可选选项表，详见 make-json-schema-validator。
;;
;; 返回值
;; ----
;; boolean
;; 满足 Schema 返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 便捷入口默认先按 2020-12 元 Schema 检查 Schema 本身。
;; 需要复用校验器或关闭元 Schema 检查时请使用 make-json-schema-validator。
;;
;; 示例
;; ----
;; (json-schema-valid? '(("name" . "Alice")) '(("type" . "object"))) => #t
;;
;; 错误处理
;; ----
;; json-schema-options-error 当选项非法时。
;; json-schema-schema-error 当 Schema 非法时。
;; json-schema-unknown-dialect 当 $schema 不是 2020-12 时。
;; json-schema-unresolvable 当引用无法解析时。

(define schema
  '(("type" . "object")
    ("properties" ("name" ("type" . "string")))
    ("required" . #("name")))
) ;define

;; 对象实例
(check-true (json-schema-valid? '(("name" . "Liii")) schema))
(check-false (json-schema-valid? '(()) schema))
(check-false (json-schema-valid? '(("name" . 1)) schema))

;; 布尔 Schema：true 接受一切，false 拒绝一切
(check-true (json-schema-valid? '(("anything" . #(1))) #t))
(check-true (json-schema-valid? 'null 'true))
(check-false (json-schema-valid? 'null #f))
(check-false (json-schema-valid? 1 'false))

;; 解析后的 JSON 文本可以直接参与校验
(define parsed-schema
  (string->json "{\"type\":\"array\",\"items\":{\"type\":\"integer\"}}")
) ;define
(check-true (json-schema-valid? (string->json "[1,2,3]") parsed-schema))
(check-false (json-schema-valid? (string->json "[1,\"x\"]") parsed-schema))

;; 未知关键字被忽略，未知方言被拒绝
(check-true (json-schema-valid? 'null '(("x-unknown" . #f)
                                        ("title" . "annotation")))
) ;check-true
(for-each
  (lambda (uri)
    (check-catch 'json-schema-unknown-dialect
      (json-schema-valid? 1 `(("$schema" . ,uri)))
    ) ;check-catch
  ) ;lambda
  '("https://json-schema.org/draft/2019-09/schema"
    "http://json-schema.org/draft-07/schema#"
    "http://json-schema.org/draft-06/schema#"
    "http://json-schema.org/draft-04/schema#"
    "http://json-schema.org/draft-03/schema#" "urn:unknown")
) ;for-each

;; 非法选项
(check-catch 'json-schema-options-error
  (json-schema-valid? 1 #t '((unknown-option . #t)))
) ;check-catch
(check-catch 'json-schema-options-error
  (json-schema-valid? 1 #t '((max-errors . -1)))
) ;check-catch
(check-catch 'json-schema-options-error
  (json-schema-valid? 1 #t '((max-errors . 0)))
) ;check-catch
(check-catch 'json-schema-options-error
  (json-schema-valid? 1 #t '((draft . draft2020-12)))
) ;check-catch
;; gf >= 18.11.47: list? 严格化为 proper-list?，点对列表 options 被拒绝。
(check-catch 'json-schema-options-error
  (json-schema-valid? 1 #t '((max-errors . 1) . bad))
) ;check-catch

;; 类型断言：每个 JSON 类型都有对应的关键字取值。

(check-true (json-schema-valid? 'null '(("type" . "null"))))
(check-true (json-schema-valid? #t '(("type" . "boolean"))))
(check-true (json-schema-valid? '(()) '(("type" . "object"))))
(check-true (json-schema-valid? #() '(("type" . "array"))))
(check-true (json-schema-valid? "x" '(("type" . "string"))))
(check-true (json-schema-valid? 1.5 '(("type" . "number"))))
(check-true (json-schema-valid? 1 '(("type" . "integer"))))
(check-true (json-schema-valid? 1.0 '(("type" . "integer"))))
(check-false (json-schema-valid? 1.5 '(("type" . "integer"))))
(check-false (json-schema-valid? #t '(("type" . "number"))))
(check-false (json-schema-valid? #f '(("type" . "integer"))))
(check-false (json-schema-valid? 'null '(("type" . "string"))))
(check-true (json-schema-valid? "1" '(("type" . #("integer" "string")))))
(check-true (json-schema-valid? 1 '(("type" . #("integer" "string")))))
(check-false (json-schema-valid? #t '(("type" . #("integer" "string")))))
(check-catch 'json-schema-schema-error
  (json-schema-valid? 1 '(("type" . "unknown")))
) ;check-catch

;; 嵌套应用关键字的回归用例。
;;
;; Goldfish 18.11.46 复用尾位置命名 let 循环的栈帧时，不会恢复外层过程
;; 的参数，导致嵌套遍历返回错误的值、路径与 Schema。
;;
;; 上游报告：https://github.com/MoganLab/goldfish/issues/1337

;; Regression tests for nested applicators that re-enter the same
;; traversal function while an outer traversal is still running.
;;
;; Goldfish 18.11.46 reuses the frame of a tail-position named-let loop
;; and does not restore the enclosing procedure's parameters when the
;; loop's argument list contains a call that re-enters the same
;; procedure. The result was silently wrong values, paths and schemas.
;;
;; Upstream report: https://github.com/MoganLab/goldfish/issues/1337

;; Nested prefixItems: the inner schema re-enters the array traversal.

(define nested-prefix-items
  (string->json "{\"$defs\": {\"item\": {\"type\": \"array\", \"prefixItems\": [{\"$ref\": \"#/$defs/sub-item\"}, {\"$ref\": \"#/$defs/sub-item\"}]}, \"sub-item\": {\"type\": \"object\", \"required\": [\"foo\"]}}, \"type\": \"array\", \"prefixItems\": [{\"$ref\": \"#/$defs/item\"}, {\"$ref\": \"#/$defs/item\"}]}"
  ) ;string->json
) ;define
(check-true (json-schema-valid? (string->json "[[{\"foo\":null},{\"foo\":null}],[{\"foo\":null},{\"foo\":null}]]"
                                ) ;string->json
              nested-prefix-items
            ) ;json-schema-valid?
) ;check-true
(check-false (json-schema-valid? (string->json "[[{\"foo\":null},{}],[{\"foo\":null},{\"foo\":null}]]")
               nested-prefix-items
             ) ;json-schema-valid?
) ;check-false

;; Nested properties: the inner schema re-enters the object traversal.

(define nested-properties
  (string->json "{\"$defs\": {\"inner\": {\"type\": \"object\", \"properties\": {\"a\": {\"type\": \"integer\"}}}}, \"type\": \"object\", \"properties\": {\"x\": {\"$ref\": \"#/$defs/inner\"}, \"y\": {\"$ref\": \"#/$defs/inner\"}}}"
  ) ;string->json
) ;define
(check-true (json-schema-valid? (string->json "{\"x\":{\"a\":1},\"y\":{\"a\":2}}")
              nested-properties
            ) ;json-schema-valid?
) ;check-true
(check-false (json-schema-valid? (string->json "{\"x\":{\"a\":1},\"y\":{\"a\":\"bad\"}}")
               nested-properties
             ) ;json-schema-valid?
) ;check-false

;; Nested allOf: the branch re-enters the combination traversal.

(define nested-all-of
  (string->json "{\"allOf\": [{\"properties\": {\"a\": {\"type\": \"integer\"}}, \"allOf\": [{\"properties\": {\"b\": {\"type\": \"integer\"}}}]}, {\"properties\": {\"c\": {\"type\": \"integer\"}}}]}"
  ) ;string->json
) ;define
(check-true (json-schema-valid? (string->json "{\"a\":1,\"b\":2,\"c\":3}") nested-all-of)
) ;check-true
(check-false (json-schema-valid? (string->json "{\"a\":1,\"b\":2,\"c\":\"bad\"}")
               nested-all-of
             ) ;json-schema-valid?
) ;check-false

;; Nested oneOf: matching counts must be tracked across re-entry.
;; Both outer branches match, so oneOf must fail.

(define nested-one-of
  (string->json "{\"oneOf\": [{\"properties\": {\"a\": {\"type\": \"integer\"}}, \"oneOf\": [{\"properties\": {\"b\": {\"type\": \"integer\"}}}]}, {\"properties\": {\"c\": {\"type\": \"integer\"}}}]}"
  ) ;string->json
) ;define
(check-false (json-schema-valid? (string->json "{\"a\":1,\"b\":2,\"c\":3}") nested-one-of)
) ;check-false
;; Only the first outer branch matches, so oneOf succeeds.

(define nested-one-of-single
  (string->json "{\"oneOf\": [{\"properties\": {\"a\": {\"type\": \"integer\"}}, \"oneOf\": [{\"properties\": {\"b\": {\"type\": \"integer\"}}}]}, {\"properties\": {\"c\": {\"type\": \"integer\"}}, \"required\": [\"missing\"]}]}"
  ) ;string->json
) ;define
(check-true (json-schema-valid? (string->json "{\"a\":1,\"b\":2}") nested-one-of-single)
) ;check-true

;; Nested anyOf: a failing branch must not contribute evaluated positions.

(define nested-any-of
  (string->json "{\"anyOf\": [{\"properties\": {\"a\": {\"type\": \"integer\"}}, \"required\": [\"missing\"]}, {\"properties\": {\"b\": {\"type\": \"integer\"}}}]}"
  ) ;string->json
) ;define
(check-true (json-schema-valid? (string->json "{\"b\":2}") nested-any-of))

;; Nested contains: matched indices must be tracked across re-entry.

(define nested-contains
  (string->json "{\"contains\": {\"type\": \"integer\"}, \"items\": {\"type\": \"integer\"}, \"unevaluatedItems\": false}"
  ) ;string->json
) ;define
(check-true (json-schema-valid? (string->json "[1,2]") nested-contains))

;; Nested dependentSchemas: properties evaluated inside the dependent
;; schema are collected, but the triggering property itself is not.

(define nested-dependent-schemas
  (string->json "{\"properties\": {\"a\": {}}, \"dependentSchemas\": {\"a\": {\"properties\": {\"b\": {\"type\": \"integer\"}}}}, \"unevaluatedProperties\": false}"
  ) ;string->json
) ;define
(check-true (json-schema-valid? (string->json "{\"a\":1,\"b\":2}") nested-dependent-schemas)
) ;check-true
(check-false (json-schema-valid? (string->json "{\"a\":1,\"b\":2,\"c\":3}")
               nested-dependent-schemas
             ) ;json-schema-valid?
) ;check-false

;; Nested unevaluatedProperties: inner branch fills the evaluated set.

(define nested-unevaluated-properties
  (string->json "{\"properties\": {\"a\": {\"type\": \"integer\"}}, \"allOf\": [{\"unevaluatedProperties\": true}], \"unevaluatedProperties\": false}"
  ) ;string->json
) ;define
(check-true (json-schema-valid? (string->json "{\"a\":1,\"b\":2}")
              nested-unevaluated-properties
            ) ;json-schema-valid?
) ;check-true

;; Deeply nested recursion must keep every path segment distinct.

(define deep-schema
  (string->json "{\"type\": \"object\", \"properties\": {\"a\": {\"type\": \"object\", \"properties\": {\"b\": {\"type\": \"object\", \"properties\": {\"c\": {\"type\": \"integer\"}}}}}}, \"additionalProperties\": false}"
  ) ;string->json
) ;define
(check-true (json-schema-valid? (string->json "{\"a\":{\"b\":{\"c\":1}}}") deep-schema)
) ;check-true
(check-false (json-schema-valid? (string->json "{\"a\":{\"b\":{\"c\":\"bad\"}}}")
               deep-schema
             ) ;json-schema-valid?
) ;check-false

(check-true (json-schema-valid? "hello" '(("type" . "string"))))
(check-false (json-schema-valid? 42 '(("type" . "string"))))
(check-true (json-schema-valid? 'true '(("type" . "boolean"))))
(check-true (json-schema-valid? #f '(("type" . "boolean"))))
(check-true (json-schema-valid? #(1 "x" null) '(("type" . "array"))))
(check-false (json-schema-valid? '() '(("type" . "object"))))

(check-true (json-schema-valid? 1 '(("const" . 1.0))))
(check-false (json-schema-valid? #t '(("const" . 1))))
(check-false (json-schema-valid? 'true '(("const" . 1))))
(check-true (json-schema-valid? #(#t 1) '(("uniqueItems" . #t))))
(check-true (json-schema-valid? #((("a" . #t)) (("a" . 1))) '(("uniqueItems"
                                                               . #t)))
)
(check-true (json-schema-valid? #((("a" . 1) ("b" . 2)) (("b" . 2) ("a" . 1)))
              '(("uniqueItems" . #f))
            ) ;json-schema-valid?
)

(check-true
  (json-schema-valid? #(1 "x" 3)
    '(("prefixItems" . #((("type" . "integer")) (("type" . "string"))))
      ("items" ("minimum" . 2)))
  ) ;json-schema-valid?
)
(check-false
  (json-schema-valid? #(1 "x" 1)
    '(("prefixItems" . #((("type" . "integer")) (("type" . "string"))))
      ("items" ("minimum" . 2)))
  ) ;json-schema-valid?
)
(check-false (json-schema-valid? #(1 2 3)
               '(("prefixItems" . #((("type" . "integer")))) ("items" . #f))
             ) ;json-schema-valid?
)
(check-false
  (json-schema-valid? #("x" 1 2) '(("items" ("type" . "integer"))))
)
(check-true
  (json-schema-valid? #(1 "x" 2) '(("contains" ("type" . "integer"))))
)
(check-false
  (json-schema-valid? #(1 "x" 2)
    '(("contains" ("type" . "integer")) ("minContains" . 3))
  ) ;json-schema-valid?
)
(check-true (json-schema-valid? 9 '(("multipleOf" . 0.3))))
(check-false (json-schema-valid? 1 '(("multipleOf" . 0.3))))
(check-true (json-schema-valid? 2 '(("minimum" . 2) ("exclusiveMinimum" . 1))))
(check-false (json-schema-valid? 2 '(("exclusiveMinimum" . 2))))
(check-true (json-schema-valid? "😀" '(("minLength" . 1) ("maxLength" . 1))))

(check-true (json-schema-valid? 4 '(("allOf"
                                     . #((("minimum" . 2)) (("maximum" . 5))))))
)
(check-true (json-schema-valid? "x"
              '(("anyOf" . #((("type" . "integer")) (("type" . "string")))))
            ) ;json-schema-valid?
)
(check-false (json-schema-valid? #t
               '(("anyOf" . #((("type" . "integer")) (("type" . "string")))))
             ) ;json-schema-valid?
)
(check-true (json-schema-valid? 4 '(("oneOf"
                                     . #((("minimum" . 2)) (("maximum" . 3))))))
)
(check-false (json-schema-valid? 4 '(("oneOf"
                                      . #((("minimum" . 2)) (("maximum" . 5))))))
)
(check-false
  (json-schema-valid? 4 '(("not" ("type" . "integer"))))
)
(check-true
  (json-schema-valid? 4 '(("if" ("minimum" . 0)) ("then" ("maximum" . 5))))
)
(check-false
  (json-schema-valid? 8 '(("if" ("minimum" . 0)) ("then" ("maximum" . 5))))
)

(check-true
  (json-schema-valid? 3
    '(("$defs" ("positive" ("minimum" . 1))) ("$ref" . "#/$defs/positive"))
  ) ;json-schema-valid?
)
(check-false
  (json-schema-valid? 0
    '(("$defs" ("positive" ("minimum" . 1))) ("$ref" . "#/$defs/positive"))
  ) ;json-schema-valid?
)
(check-catch 'json-schema-unresolvable
  (json-schema-valid? 1 '(("$ref" . "#/missing")))
)

(check-true
  (json-schema-valid? 2
    '(("$defs" ("unused" . #f) ("positive" ("minimum" . 1)))
      ("$ref" . "#/$defs/positive"))
  ) ;json-schema-valid?
)

(define person-schema
  '(("type" . "object")
    ("properties"
     ("name" ("type" . "string"))
     ("age" ("type" . "integer") ("minimum" . 0)))
    ("required" . #("name" "age"))
    ("additionalProperties" . #f))
)

(define person-schema
  '(("type" . "object")
    ("properties"
     ("name" ("type" . "string"))
     ("age" ("type" . "integer") ("minimum" . 0)))
    ("required" . #("name" "age"))
    ("additionalProperties" . #f))
)
(check-true (json-schema-valid? '(("name" . "Alice") ("age" . 18)) person-schema)
)
(check-false (json-schema-valid? '(("name" . "Alice")) person-schema))
(check-false (json-schema-valid? '(("name" . "Alice") ("age" . -1)) person-schema)
)
(check-false (json-schema-valid? '(("name" . "Alice") ("age" . 18) ("x" . 1)) person-schema)
)

(define lazy-calls 0)
(define lazy-checker (lambda (value) (set! lazy-calls (+ lazy-calls 1)) #t))

(define lazy-options `((formats . #t) (format-checkers (even . ,lazy-checker))))
(check-false (json-schema-valid? "x"
               '(("type" . "integer") ("format" . "even"))
               lazy-options
             ) ;json-schema-valid?
)
(check lazy-calls => 0)
(check
  (length
    (json-schema-errors "x"
      '(("type" . "integer") ("format" . "even"))
      `((max-errors . 1) ,@lazy-options)
    ) ;json-schema-errors
  ) ;length
  =>
  1
)
(check lazy-calls => 0)
(check-true (json-schema-valid? 2
              '(("anyOf" . #((("minimum" . 0)) (("format" . "even")))))
              lazy-options
            ) ;json-schema-valid?
)
(check lazy-calls => 0)

(check-report)
