(import (liii json schema) (liii json) (liii check) (liii base))

(check-set-mode! 'report-failed)

;; Entry point and basic JSON types.
(check-true (json-schema-valid? "hello" '(("type" . "string"))))
(check-false (json-schema-valid? 42 '(("type" . "string"))))
(check-true (json-schema-valid? 'null '(("type" . "null"))))
(check-true (json-schema-valid? 'true '(("type" . "boolean"))))
(check-true (json-schema-valid? #f '(("type" . "boolean"))))
(check-true (json-schema-valid? #(1 "x" null) '(("type" . "array"))))
(check-true (json-schema-valid? '(()) '(("type" . "object"))))
(check-false (json-schema-valid? '() '(("type" . "object"))))
(check-true (json-schema-valid? 1 '(("type" . #("integer" "string")))))

;; Boolean schemas support both Goldfish booleans and string->json booleans.
(check-true (json-schema-valid? '(("anything" . #(1))) #t))
(check-true (json-schema-valid? 'null 'true))
(check-false (json-schema-valid? 'null #f))
(check-false (json-schema-valid? 1 'false))

;; Parsed schemas and instances work without a separate conversion step.

(define parsed-schema
  (string->json "{\"type\":\"array\",\"items\":{\"type\":\"integer\"}}")
) ;define
(check-true (json-schema-valid? (string->json "[1,2,3]") parsed-schema))
(check-false (json-schema-valid? (string->json "[1,\"x\"]") parsed-schema))

;; JSON Schema 2020-12 integer behavior and dialect rejection.
(check-true (json-schema-valid? 1.0 '(("type" . "integer"))))
(check-catch 'json-schema-options-error
  (json-schema-valid? 1 #t '((draft . draft2020-12)))
) ;check-catch
;; gf >= 18.11.47: list? 严格化为 proper-list?，点对列表 options 被拒绝。
(check-catch 'json-schema-options-error
  (json-schema-valid? 1 #t '((max-errors . 1) . bad))
) ;check-catch
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

;; JSON equality differs from Scheme numeric/boolean equality.
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

;; Objects and nested paths.

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
(let ((errors (json-schema-errors '(("name" . 7)) person-schema)))
  (check-true (= (length errors) 2))
  (check
    (json-schema-error-json-path
      (car
        (json-schema-validator-errors
          (make-json-schema-validator
            '(("properties" ("a" ("type" . "string"))))
          ) ;make-json-schema-validator
          '((a . 1))
        ) ;json-schema-validator-errors
      ) ;car
    ) ;json-schema-error-json-path
    =>
    "$.a"
  ) ;check
) ;let
(check-true
  (json-schema-valid? '((abc . "ok")) '(("propertyNames" ("minLength" . 3))))
) ;check-true

;; Arrays, numeric and string assertions.
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

;; Applicators and short-circuit-compatible behavior.
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

;; Local JSON Pointer references and immutable registries.
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

(define empty-registry (make-json-schema-registry))

(define registry
  (json-schema-registry-add empty-registry "urn:positive" '(("minimum" . 1)))
) ;define
(check-true (json-schema-registry? empty-registry))
(check-catch 'json-schema-unresolvable
  (json-schema-valid? 0 '(("$ref" . "urn:positive")))
) ;check-catch
(check-true
  (json-schema-valid? 3 '(("$ref" . "urn:positive")) `((registry . ,registry)))
) ;check-true

;; Reusable validators, errors, max-errors, and thrown error category.

(define validator
  (make-json-schema-validator person-schema '((check-schema? . #t)))
) ;define
(check-true (json-schema-validator? validator))
(check-true (json-schema-validator-valid? validator '(("name" . "A")
                                                      ("age" . 1)))
) ;check-true
(check-false (json-schema-validator-valid? validator '(("name" . "A"))))
(check (json-schema-validator-validate validator '(("name" . "A") ("age" . 1)))
  =>
  '(("name" . "A") ("age" . 1))
) ;check
(check-catch 'json-schema-validation-error
  (json-schema-validator-validate validator '(("name" . "A")))
) ;check-catch
(check
  (length (json-schema-errors '(("name" . 7)) person-schema '((max-errors . 1))))
  =>
  1
) ;check
(let ((err (car (json-schema-errors 0 person-schema))))
  (check-true (json-schema-error? err))
  (check (json-schema-error-kind err) => 'validation)
  (check (json-schema-error-keyword err) => "type")
  (check (json-schema-error-path err) => '())
) ;let

;; Core traversal, boolean schemas and unknown keywords.
(check-true (json-schema-valid? 'null '(("x-unknown" . #f)
                                        ("title" . "annotation")))
) ;check-true

(define traversal-schema
  '(("properties" ("outer" ("items" ("properties" ("inner" . #f))))))
) ;define

(define traversal-instance '(("outer" . #((("inner" . 1))))))
(let ((error (car (json-schema-errors traversal-instance traversal-schema))))
  (check (json-schema-error-path error) => '("outer" 0 "inner"))
  (check (json-schema-error-schema-path error)
    =>
    '("properties" "outer" "items" "properties" "inner")
  ) ;check
  (check (json-schema-error-json-path error) => "$.outer[0].inner")
) ;let

;; Local references are entered on demand without walking $defs as assertions.
(check-true
  (json-schema-valid? 2
    '(("$defs" ("unused" . #f) ("positive" ("minimum" . 1)))
      ("$ref" . "#/$defs/positive"))
  ) ;json-schema-valid?
) ;check-true

;; First-error mode and lazy combination evaluation.

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
(check-catch 'json-schema-options-error
  (json-schema-valid? 1 #t '((max-errors . 0)))
) ;check-catch

(check-report)
