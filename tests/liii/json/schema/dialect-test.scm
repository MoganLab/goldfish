(import (liii json schema)
  (liii json schema dialect)
  (liii json schema meta)
  (liii json schema data)
  (liii check)
  (scheme base)
) ;import
(check-set-mode! 'report-failed)

(check (draft-for #t) => 'draft2020-12)
(check (draft-for '(())) => 'draft2020-12)
(check (draft-for '(("type" . "integer"))) => 'draft2020-12)
(check (draft-for '(("$schema" . "https://json-schema.org/draft/2020-12/schema")))
  =>
  'draft2020-12
) ;check
(check (draft-for '(("$schema"
                     . "https://json-schema.org/draft/2020-12/schema#")))
  =>
  'draft2020-12
) ;check

(for-each
  (lambda (uri)
    (check-catch 'json-schema-unknown-dialect (draft-for `(("$schema" . ,uri))))
  ) ;lambda
  '("https://json-schema.org/draft/2019-09/schema"
    "http://json-schema.org/draft-07/schema#"
    "http://json-schema.org/draft-06/schema#"
    "http://json-schema.org/draft-04/schema#"
    "http://json-schema.org/draft-03/schema#" "urn:example:unknown")
) ;for-each

(for-each
  (lambda (value)
    (check-catch 'json-schema-schema-error (draft-for `(("$schema" . ,value))))
  ) ;lambda
  '(#f 42 ())
) ;for-each

(check-catch 'json-schema-options-error
  (json-schema-valid? 1 #t '((draft . draft2020-12)))
) ;check-catch
;; Embedded meta-schema resources are complete and addressable offline.
(check (length (json-schema-meta-uris)) => 9)
(for-each
  (lambda (uri)
    (let ((resource (json-schema-meta-resource uri)))
      (check-true (pair? resource))
      (check (object-ref resource "$id" #f) => uri)
    ) ;let
  ) ;lambda
  (json-schema-meta-uris)
) ;for-each
(check-true (pair? (object-ref (json-schema-meta-resource "https://json-schema.org/draft/2020-12/schema")
                     "$vocabulary"
                     #f
                   ) ;object-ref
            ) ;pair?
) ;check-true
(check-catch 'json-schema-unresolvable
  (json-schema-meta-resource "urn:missing")
) ;check-catch

(check-report)
