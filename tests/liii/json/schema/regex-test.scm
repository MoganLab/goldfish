(import (liii json schema regex) (liii json schema) (liii check) (scheme base))
(check-set-mode! 'report-failed)

;; The common backend provides search, not implicit full-string matching.
(check-true (regex-search? "abc" "xxabcxx"))
(check-false (regex-search? "^abc$" "xxabcxx"))
(check-true (regex-search? "^abc$" "abc"))
(check-true (regex-search? "😀" "x😀x"))
(check-true (regex-search? "a+" "caaat"))
(check-false (regex-search? "a; echo injected" "a"))
(check-catch 'json-schema-schema-error (regex-search? "[" "x"))

(check-true (json-schema-valid? "xxabcxx" '(("pattern" . "abc"))))
(check-false (json-schema-valid? "xxabcxx" '(("pattern" . "^abc$"))))
(check-false (json-schema-valid? "xyz" '(("pattern" . "abc"))))
(check-true (json-schema-valid? 42 '(("pattern" . "abc"))))
(check-catch 'json-schema-schema-error
  (json-schema-valid? "x" '(("pattern" . "[")))
) ;check-catch

;; Every matching patternProperties branch is evaluated.
(check-true
  (json-schema-valid? '(("alpha" . 1) ("beta" . "ok"))
    '(("patternProperties"
       ("^a" ("type" . "integer"))
       ("pha$" ("type" . "integer"))
       ("^b" ("type" . "string"))))
  ) ;json-schema-valid?
) ;check-true
(check-false
  (json-schema-valid? '(("alpha" . 1))
    '(("patternProperties"
       ("^a" ("type" . "integer"))
       ("pha$" ("type" . "string"))))
  ) ;json-schema-valid?
) ;check-false

;; Matched properties are not additionalProperties.
(check-true
  (json-schema-valid? '(("alpha" . 1))
    '(("patternProperties" ("^a" ("type" . "integer")))
      ("additionalProperties" . #f))
  ) ;json-schema-valid?
) ;check-true
(check-false
  (json-schema-valid? '(("beta" . 1))
    '(("patternProperties" ("^a" ("type" . "integer")))
      ("additionalProperties" . #f))
  ) ;json-schema-valid?
) ;check-false
(check-report)
