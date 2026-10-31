(import (liii json schema data)
  (liii json schema)
  (liii json)
  (liii check)
  (scheme base)
) ;import

(check-set-mode! 'report-failed)

;; String and symbol keys share lookup semantics without losing the stored key.
(let* ((object '(("name" . "Liii") (age . 18) ("enabled" . #f)))
       (cell (object-cell object 'name))
      ) ;
  (check-true (pair? cell))
  (check (car cell) => "name")
  (check (object-ref object "age" #f) => 18)
  (check-true (object-has? object 'enabled))
  (check-false (object-ref object "enabled" #t))
  (check-false (object-has? object "missing"))
  (check (object-ref object "missing" 'fallback) => 'fallback)
) ;let*

;; The empty object has no entries and is different from the empty list and array.
(check (entries '(())) => '())
(check (entries '()) => '())
(check-true (json-object? '(())))
(check-false (json-object? '()))
(check-true (vector? #()))
(check (length (listify #())) => 0)
(check (listify '(())) => '(()))

;; JSON values retain their distinct public types.
(check-true (json-schema-valid? 'null '(("type" . "null"))))
(check-false (json-schema-valid? 'null '(("type" . "boolean"))))
(check-true (json-schema-valid? #f '(("type" . "boolean"))))
(check-false (json-schema-valid? #f '(("type" . "integer"))))
(check-false (json-schema-valid? 0 '(("type" . "boolean"))))
(check-true (json-schema-valid? 0 '(("type" . "integer"))))

;; Options and sequence normalization are shared by all modules.
(let ((options '((formats . #f) (max-errors . 2))))
  (check-false (option-ref options 'formats #t))
  (check (option-ref options 'max-errors #f) => 2)
  (check (option-ref options 'registry 'missing) => 'missing)
) ;let
(check (listify #(1 2)) => '(1 2))
(check (listify 1) => '(1))
(check
  (collect-for-each (lambda (x) (list x (* x 2))) '(1 2))
  =>
  '(1 2 2 4)
) ;check

(check-catch 'type-error (key->string 1))
(check-catch 'type-error (object-ref '() 1 #f))
(check-report)
