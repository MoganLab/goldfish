(import (liii json schema) (liii check) (scheme base))
(check-set-mode! 'report-failed)

(check-true (json-schema-valid? 2 '(("minimum" . 2))))
(check-false (json-schema-valid? 1.999 '(("minimum" . 2))))
(check-true (json-schema-valid? 2 '(("maximum" . 2))))
(check-false (json-schema-valid? 2.001 '(("maximum" . 2))))
(check-true (json-schema-valid? 3 '(("exclusiveMinimum" . 2))))
(check-false (json-schema-valid? 2 '(("exclusiveMinimum" . 2))))
(check-true (json-schema-valid? 1 '(("exclusiveMaximum" . 2))))
(check-false (json-schema-valid? 2 '(("exclusiveMaximum" . 2))))
(check-true (json-schema-valid? "x" '(("minimum" . 2))))
(check-true (json-schema-valid? #t '(("multipleOf" . 2))))

(check-true (json-schema-valid? 9 '(("multipleOf" . 0.3))))
(check-false (json-schema-valid? 1 '(("multipleOf" . 0.3))))
(check-true (json-schema-valid? 0.0075 '(("multipleOf" . 0.0001))))
(check-false (json-schema-valid? 0.00751 '(("multipleOf" . 0.0001))))
(check-true (json-schema-valid? 12345678900 '(("multipleOf" . 1234567890))))
(check-false (json-schema-valid? 1e+308 '(("multipleOf" . 0.123456789))))
(check-catch 'json-schema-schema-error
  (json-schema-valid? 1 '(("multipleOf" . 0)))
) ;check-catch
;; String lengths count Unicode code points, not UTF-8 bytes or graphemes.
(check-true (json-schema-valid? "" '(("minLength" . 0) ("maxLength" . 0))))
(check-true (json-schema-valid? "😀" '(("minLength" . 1) ("maxLength" . 1))))
(check-false (json-schema-valid? "😀" '(("minLength" . 2))))
(check-true (json-schema-valid? "中文" '(("minLength" . 2) ("maxLength" . 2)))
) ;check-true
(check-true (json-schema-valid? "é" '(("minLength" . 2) ("maxLength" . 2))))
(check-false (json-schema-valid? "é" '(("maxLength" . 1))))
(check-true (json-schema-valid? 42 '(("minLength" . 100))))
(check-true (json-schema-valid? #f '(("maxLength" . 0))))
(check-true (json-schema-valid? 'null '(("minLength" . 1))))

;; Object applicators keep additionalProperties scoped to sibling keywords.
(check-true
  (json-schema-valid? '(("a" . 1))
    '(("properties" ("a" ("type" . "integer"))) ("additionalProperties" . #f))
  ) ;json-schema-valid?
) ;check-true
(check-false (json-schema-valid? '(("a" . 1))
               '(("allOf" . #((("properties" ("a" ("type" . "integer"))))))
                 ("additionalProperties" . #f))
             ) ;json-schema-valid?
) ;check-false
(check-true (json-schema-valid? '(("a" . #f)) '(("required" . #("a")))))
(check-false
  (json-schema-valid? '(("a" . #f)) '(("dependentRequired" ("a" . #("b")))))
) ;check-false
(check-true
  (json-schema-valid? '(("a" . 1) ("b" . 2))
    '(("dependentSchemas" ("a" ("required" . #("b")))))
  ) ;json-schema-valid?
) ;check-true
(check-false
  (json-schema-valid? '(("a" . 1))
    '(("dependentSchemas" ("a" ("required" . #("b")))))
  ) ;json-schema-valid?
) ;check-false

;; Arrays handle short prefixes, contains bounds and nested error paths.
(check-true (json-schema-valid? #(1)
              '(("prefixItems"
                 . #((("type" . "integer")) (("type" . "integer"))))
                ("items" . #f))
            ) ;json-schema-valid?
) ;check-true
(check-true
  (json-schema-valid? #()
    '(("contains" ("const" . 1)) ("minContains" . 0) ("maxContains" . 0))
  ) ;json-schema-valid?
) ;check-true
(check-false
  (json-schema-valid? #(1)
    '(("contains" ("const" . 1)) ("minContains" . 0) ("maxContains" . 0))
  ) ;json-schema-valid?
) ;check-false
(check-true (json-schema-valid? #(1 2) '(("minContains" . 0)
                                         ("maxContains" . 0)))
) ;check-true
(let ((error
        (car
          (json-schema-errors #(#("x")) '(("items"
                                           ("items" ("type" . "integer")))))
        ) ;car
      ) ;error
     ) ;
  (check (json-schema-error-path error) => '(0 0))
) ;let

;; Conditional applicators select exactly one branch without exposing if probe errors.
(check-false (json-schema-valid? 1 '(("if" . #t) ("then" . #f) ("else" . #t))))
(check-true (json-schema-valid? 1 '(("if" . #f) ("then" . #f) ("else" . #t))))
(check-true (json-schema-valid? 1 '(("if" . #f))))
(check-true
  (json-schema-valid? 1 '(("if" ("type" . "string"))))
) ;check-true
(check-false
  (json-schema-valid? 2
    '(("if" ("minimum" . 0)) ("then" ("if" ("minimum" . 2)) ("then" . #f)))
  ) ;json-schema-valid?
) ;check-false
(let ((error
        (car
          (json-schema-errors "x"
            '(("if" ("type" . "integer")) ("else" ("type" . "number")))
          ) ;json-schema-errors
        ) ;car
      ) ;error
     ) ;
  (check (json-schema-error-keyword error) => "type")
  (check (json-schema-error-schema-path error) => '("else" "type"))
) ;let

(check-report)
