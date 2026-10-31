(import (liii json schema) (liii check))

(check-set-mode! 'report-failed)

(define schema
  '(("type" . "object")
    ("properties" ("name" ("type" . "string")))
    ("required" . #("name")))
) ;define

;; Main-entry convenience API.
(check-true (json-schema-valid? '(("name" . "Liii")) schema))
(check-false (json-schema-valid? '(()) schema))
(check (json-schema-validate '(("name" . "Liii")) schema)
  =>
  '(("name" . "Liii"))
) ;check
(check-catch 'json-schema-validation-error (json-schema-validate '(()) schema))
(check (json-schema-check-schema schema) => schema)
(check-true
  (= (length (json-schema-errors '(()) schema)) 1)
) ;check-true

;; Reusable validator API.

(define validator (make-json-schema-validator schema))
(check-true (json-schema-validator? validator))
(check-true (json-schema-validator-valid? validator '(("name" . "Liii"))))
(check-false (json-schema-validator-valid? validator '(("name" . 1))))
(check-true
  (=
    (length (json-schema-validator-errors validator '(("name" . 1))))
    1
  ) ;=
) ;check-true
(check-catch 'json-schema-validation-error
  (json-schema-validator-validate validator '(("name" . 1)))
) ;check-catch

;; Structured errors and immutable registries are available from the same entry.
(let ((error (car (json-schema-errors '(("name" . 1)) schema))))
  (check-true (json-schema-error? error))
  (check (json-schema-error-kind error) => 'validation)
  (check (json-schema-error-keyword error) => "type")
  (check (json-schema-error-path error) => '("name"))
  (check (json-schema-error-absolute-path error) => '("name"))
  (check (json-schema-error-json-path error) => "$.name")
) ;let

(define empty (make-json-schema-registry))

(define filled
  (json-schema-registry-add-many empty '(("urn:string" ("type" . "string"))))
) ;define
(check-true (json-schema-registry? filled))
(check-true
  (json-schema-valid? "x" '(("$ref" . "urn:string")) `((registry . ,filled)))
) ;check-true
(check-catch 'json-schema-options-error
  (json-schema-registry-add filled "urn:string" #t)
) ;check-catch

;; Options are validated once and retained by each reusable validator.
(check-catch 'json-schema-options-error
  (json-schema-valid? 1 #t '((unknown-option . #t)))
) ;check-catch
(check-catch 'json-schema-options-error
  (json-schema-valid? 1 #t '((max-errors . -1)))
) ;check-catch
(check-catch 'json-schema-options-error
  (make-json-schema-validator #t '((registry)))
) ;check-catch
(check-catch 'json-schema-options-error
  (make-json-schema-validator #t '((retrieve . "not-a-procedure")))
) ;check-catch
(check-catch 'json-schema-options-error (make-json-schema-validator #t '() '()))

(define reusable
  (make-json-schema-validator '(("type" . "integer")) '((max-errors . 1)))
) ;define
(check-true (json-schema-validator-valid? reusable 1))
(check-true (json-schema-validator-valid? reusable 2))
(check-false (json-schema-validator-valid? reusable "2"))

;; Full meta-schema validation catches malformed keyword values.
(check (json-schema-check-schema #t) => #t)
(check (json-schema-check-schema #f) => #f)
(check-catch 'json-schema-schema-error
  (json-schema-check-schema '(("type" . 1)))
) ;check-catch
(check-catch 'json-schema-schema-error
  (json-schema-check-schema '(("required" . "x")))
) ;check-catch
(check-catch 'json-schema-schema-error
  (json-schema-check-schema '(("minimum" . "0")))
) ;check-catch
(check-catch 'json-schema-schema-error
  (json-schema-check-schema '(("prefixItems" . #t)))
) ;check-catch
(check-catch 'json-schema-schema-error
  (make-json-schema-validator '(("type" . 1)) '((check-schema? . #t)))
) ;check-catch
(check-true
  (json-schema-validator? (make-json-schema-validator '(("type" . 1))))
) ;check-true

(check-report)
