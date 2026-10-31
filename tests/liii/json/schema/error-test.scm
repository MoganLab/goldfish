(import (liii json schema) (liii json schema error) (liii check) (scheme base))
(check-set-mode! 'report-failed)

(define schema
  '(("properties"
     ("rows" ("items" ("properties" ("name" ("type" . "string"))))))
    ("additionalProperties" . #f))
) ;define

(define instance '(("rows" . #((("name" . 7))))))
(let ((error (car (json-schema-errors instance schema))))
  (check-true (json-schema-error? error))
  (check (json-schema-error-kind error) => 'validation)
  (check-true (string? (json-schema-error-message error)))
  (check (json-schema-error-keyword error) => "type")
  (check (json-schema-error-keyword-value error) => "string")
  (check (json-schema-error-instance error) => 7)
  (check-true (pair? (json-schema-error-schema error)))
  (check (json-schema-error-path error) => '("rows" 0 "name"))
  (check (json-schema-error-absolute-path error) => '("rows" 0 "name"))
  (check (json-schema-error-schema-path error)
    =>
    '("properties" "rows" "items" "properties" "name" "type")
  ) ;check
  (check (json-schema-error-absolute-schema-path error)
    =>
    '("properties" "rows" "items" "properties" "name" "type")
  ) ;check
  (check (json-schema-error-context error) => '())
  (check-false (json-schema-error-cause error))
  (check (json-schema-error-json-path error) => "$.rows[0].name")
) ;let

;; Combination errors retain child context and complete paths.
(let* ((errors (json-schema-errors 1 '(("anyOf"
                                        . #((("type" . "string"))
                                            (("minimum" . 2))
                                          ) ;#
                                       )))
       ) ;errors
       (parent (car errors))
       (children (json-schema-error-context parent))
      ) ;
  (check (json-schema-error-keyword parent) => "anyOf")
  (check (length children) => 2)
  (check (json-schema-error-keyword (cadr children)) => "minimum")
) ;let*

;; Explicit record fields are stable, including context and cause.
(let* ((cause (list 'underlying))
       (child (make-validation-error 'validation
                "child"
                "type"
                "string"
                1
                #t
                '("x")
                '("type")
                '()
                #f
              ) ;make-validation-error
       ) ;child
       (error (make-validation-error 'schema
                "parent"
                "anyOf"
                #()
                1
                #t
                '()
                '("anyOf")
                (list child)
                cause
              ) ;make-validation-error
       ) ;error
      ) ;
  (check (json-schema-error-kind error) => 'schema)
  (check (json-schema-error-cause error) => cause)
  (check (length (json-schema-error-context error)) => 1)
  (check (json-schema-error-absolute-path child) => '("x"))
) ;let*

;; JSON Path uses dot notation only for identifiers and escapes other keys.
(let ((error
        (car
          (json-schema-errors '(("a'b\\c" . 1))
            '(("properties" ("a'b\\c" ("type" . "string"))))
          ) ;json-schema-errors
        ) ;car
      ) ;error
     ) ;
  (check (json-schema-error-json-path error) => "$['a\\'b\\\\c']")
) ;let

(check-catch 'json-schema-validation-error (json-schema-validate 1 #f))
(check-catch 'json-schema-schema-error (json-schema-valid? 1 '()))
(check-catch 'json-schema-unknown-dialect
  (json-schema-valid? 1 '(("$schema" . "urn:unknown")))
) ;check-catch
(check-catch 'json-schema-unresolvable
  (json-schema-valid? 1 '(("$ref" . "#/missing")))
) ;check-catch
(check-report)
