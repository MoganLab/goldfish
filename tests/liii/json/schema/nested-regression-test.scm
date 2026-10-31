;; 嵌套应用关键字的回归测试（独立成文件：json-schema-valid-p-test.scm
;; 聚合后超过 gf check 的 400 行上限）。
;;
;; Goldfish 18.11.46 复用尾位置命名 let 循环的栈帧时，不会恢复外层过程
;; 的参数，导致嵌套遍历返回错误的值、路径与 Schema。
;;
;; 上游报告：https://github.com/MoganLab/goldfish/issues/1337

(import (liii json schema) (liii json) (liii check) (scheme base))

(check-set-mode! 'report-failed)

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
) ;check-true
(check-true (json-schema-valid? #((("a" . 1) ("b" . 2)) (("b" . 2) ("a" . 1)))
              '(("uniqueItems" . #f))
            ) ;json-schema-valid?
) ;check-true

(check-true
  (json-schema-valid? #(1 "x" 3)
    '(("prefixItems" . #((("type" . "integer")) (("type" . "string"))))
      ("items" ("minimum" . 2)))
  ) ;json-schema-valid?
) ;check-true
(check-false
  (json-schema-valid? #(1 "x" 1)
    '(("prefixItems" . #((("type" . "integer")) (("type" . "string"))))
      ("items" ("minimum" . 2)))
  ) ;json-schema-valid?
) ;check-false
(check-false (json-schema-valid? #(1 2 3)
               '(("prefixItems" . #((("type" . "integer")))) ("items" . #f))
             ) ;json-schema-valid?
) ;check-false
(check-false
  (json-schema-valid? #("x" 1 2) '(("items" ("type" . "integer"))))
) ;check-false
(check-true
  (json-schema-valid? #(1 "x" 2) '(("contains" ("type" . "integer"))))
) ;check-true
(check-false
  (json-schema-valid? #(1 "x" 2)
    '(("contains" ("type" . "integer")) ("minContains" . 3))
  ) ;json-schema-valid?
) ;check-false
(check-true (json-schema-valid? 9 '(("multipleOf" . 0.3))))
(check-false (json-schema-valid? 1 '(("multipleOf" . 0.3))))
(check-true (json-schema-valid? 2 '(("minimum" . 2) ("exclusiveMinimum" . 1))))
(check-false (json-schema-valid? 2 '(("exclusiveMinimum" . 2))))
(check-true (json-schema-valid? "😀" '(("minLength" . 1) ("maxLength" . 1))))

(check-true (json-schema-valid? 4 '(("allOf"
                                     . #((("minimum" . 2)) (("maximum" . 5))))))
) ;check-true
(check-true (json-schema-valid? "x"
              '(("anyOf" . #((("type" . "integer")) (("type" . "string")))))
            ) ;json-schema-valid?
) ;check-true
(check-false (json-schema-valid? #t
               '(("anyOf" . #((("type" . "integer")) (("type" . "string")))))
             ) ;json-schema-valid?
) ;check-false
(check-true (json-schema-valid? 4 '(("oneOf"
                                     . #((("minimum" . 2)) (("maximum" . 3))))))
) ;check-true
(check-false (json-schema-valid? 4 '(("oneOf"
                                      . #((("minimum" . 2)) (("maximum" . 5))))))
) ;check-false
(check-false
  (json-schema-valid? 4 '(("not" ("type" . "integer"))))
) ;check-false
(check-true
  (json-schema-valid? 4 '(("if" ("minimum" . 0)) ("then" ("maximum" . 5))))
) ;check-true
(check-false
  (json-schema-valid? 8 '(("if" ("minimum" . 0)) ("then" ("maximum" . 5))))
) ;check-false

(check-true
  (json-schema-valid? 3
    '(("$defs" ("positive" ("minimum" . 1))) ("$ref" . "#/$defs/positive"))
  ) ;json-schema-valid?
) ;check-true
(check-false
  (json-schema-valid? 0
    '(("$defs" ("positive" ("minimum" . 1))) ("$ref" . "#/$defs/positive"))
  ) ;json-schema-valid?
) ;check-false
(check-catch 'json-schema-unresolvable
  (json-schema-valid? 1 '(("$ref" . "#/missing")))
) ;check-catch

(check-true
  (json-schema-valid? 2
    '(("$defs" ("unused" . #f) ("positive" ("minimum" . 1)))
      ("$ref" . "#/$defs/positive"))
  ) ;json-schema-valid?
) ;check-true

(define person-schema
  '(("type" . "object")
    ("properties"
     ("name" ("type" . "string"))
     ("age" ("type" . "integer") ("minimum" . 0)))
    ("required" . #("name" "age"))
    ("additionalProperties" . #f))
) ;define

(define person-schema
  '(("type" . "object")
    ("properties"
     ("name" ("type" . "string"))
     ("age" ("type" . "integer") ("minimum" . 0)))
    ("required" . #("name" "age"))
    ("additionalProperties" . #f))
) ;define
(check-true (json-schema-valid? '(("name" . "Alice") ("age" . 18)) person-schema)
) ;check-true
(check-false (json-schema-valid? '(("name" . "Alice")) person-schema))
(check-false (json-schema-valid? '(("name" . "Alice") ("age" . -1)) person-schema)
) ;check-false
(check-false (json-schema-valid? '(("name" . "Alice") ("age" . 18) ("x" . 1)) person-schema)
) ;check-false

(define lazy-calls 0)

(define lazy-checker (lambda (value) (set! lazy-calls (+ lazy-calls 1)) #t))

(define lazy-options `((formats . #t) (format-checkers (even . ,lazy-checker))))
(check-false (json-schema-valid? "x"
               '(("type" . "integer") ("format" . "even"))
               lazy-options
             ) ;json-schema-valid?
) ;check-false
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
) ;check
(check lazy-calls => 0)
(check-true (json-schema-valid? 2
              '(("anyOf" . #((("minimum" . 0)) (("format" . "even")))))
              lazy-options
            ) ;json-schema-valid?
) ;check-true
(check lazy-calls => 0)

(check-report)
