(import (liii json schema)
  (liii json schema core)
  (liii json schema evaluated)
  (liii json)
  (liii check)
  (scheme base)
) ;import
(check-set-mode! 'report-failed)

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

(check-report)
