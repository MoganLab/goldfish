(import (liii json schema)
  (liii json schema cache)
  (liii json schema reference)
  (liii check)
  (scheme base)
) ;import
(check-set-mode! 'report-failed)

(define root-a '(("$id" . "https://example.com/a/root")))

(define root-b '(("$id" . "https://example.com/b/root")))

;; 1. A key matches only when reference, root and scope all match.

(define key-a (list "child.json" root-a '()))

(define key-b (list "child.json" root-b '()))

(define key-scope (list "child.json" root-a (list root-a)))

(define (matches? entry key)
  (reference-cache-key-matches? entry (car key) (cadr key) (caddr key))
) ;define

(check-true (matches? key-a key-a))
(check-false (matches? key-a key-b))
(check-false (matches? key-a key-scope))
(check-false (matches? key-b key-a))

;; 2. The cache stores and returns the value of a matching key.

(define cache (make-reference-cache))
(check-false (reference-cache-lookup cache "child.json" root-a '()))
(reference-cache-put! cache "child.json" root-a '() 'VALUE)

(define entry (reference-cache-lookup cache "child.json" root-a '()))
(check (if entry #t #f) => #t)
(check (cadddr entry) => 'VALUE)

;; 3. A different root or a different scope does not hit the entry.
(check-false (reference-cache-lookup cache "child.json" root-b '()))
(check-false (reference-cache-lookup cache "child.json" root-a (list root-a)))
(check (if (reference-cache-lookup cache "child.json" root-a '()) #t #f) => #t)

;; 4. A different reference does not hit the entry.
(check-false (reference-cache-lookup cache "other.json" root-a '()))

;; 5. A cached false schema is still a hit.

(define false-cache (make-reference-cache))
(reference-cache-put! false-cache "false.json" root-a '() #f)

(define false-entry
  (reference-cache-lookup false-cache "false.json" root-a '())
) ;define
(check (if false-entry #t #f) => #t)
(check (cadddr false-entry) => #f)

;; 6. A resource obtained through retrieve is never cached, so the
;;    retrieve function keeps being called for every resolution.

(define retrieve-calls 0)

(define remote-resource '(("type" . "integer")))

(define (counting-retrieve uri)
  (set! retrieve-calls (+ retrieve-calls 1))
  remote-resource
) ;define

(define remote-validator
  (make-json-schema-validator '(("$ref" . "urn:counted"))
    `((retrieve . ,counting-retrieve) (check-schema? . #f))
  ) ;make-json-schema-validator
) ;define
(check-true (json-schema-validator-valid? remote-validator 1))
(check-true (json-schema-validator-valid? remote-validator 1))
(check-true (json-schema-validator-valid? remote-validator 1))
(check retrieve-calls => 3)

;; 7. A registry resource is cached and resolves to the same target.

(define cached-registry
  (json-schema-registry-add (make-json-schema-registry)
    "urn:cached"
    '(("type" . "integer"))
  ) ;json-schema-registry-add
) ;define

(define registry-validator
  (make-json-schema-validator '(("$ref" . "urn:cached"))
    `((registry . ,cached-registry) (check-schema? . #f))
  ) ;make-json-schema-validator
) ;define
(check-true (json-schema-validator-valid? registry-validator 1))
(check-true (json-schema-validator-valid? registry-validator 1))
(check-true (json-schema-validator-valid? registry-validator 1))

;; 8. The same relative reference under two different base resources
;;    stays separate, so the cached entry never crosses resources.

(define base-a
  '(("$id" . "https://example.com/a/root")
    ("$defs" ("x" ("$id" . "x.json") ("const" . 1)))
    ("$ref" . "x.json"))
) ;define

(define base-b
  '(("$id" . "https://example.com/b/root")
    ("$defs" ("x" ("$id" . "x.json") ("const" . 2)))
    ("$ref" . "x.json"))
) ;define
(check-true (json-schema-valid? 1 base-a '((check-schema? . #f))))
(check-false (json-schema-valid? 2 base-a '((check-schema? . #f))))
(check-true (json-schema-valid? 2 base-b '((check-schema? . #f))))
(check-false (json-schema-valid? 1 base-b '((check-schema? . #f))))

;; 9. Two independent validators do not share cached entries.

(define shared-schema '(("$defs" ("x" ("const" . 7))) ("$ref" . "#/$defs/x")))

(define validator-one
  (make-json-schema-validator shared-schema '((check-schema? . #f)))
) ;define

(define validator-two
  (make-json-schema-validator shared-schema '((check-schema? . #f)))
) ;define
(check-true (json-schema-validator-valid? validator-one 7))
(check-true (json-schema-validator-valid? validator-two 7))
(check-false (json-schema-validator-valid? validator-one 8))

;; 10. valid? stops at the first failing keyword and does not evaluate
;;     the later ones.

(define probe-calls 0)

(define (counting-format value)
  (set! probe-calls (+ probe-calls 1))
  #t
) ;define

(define short-circuit-schema
  '(("type" . "string") ("format" . "probe") ("minLength" . 10))
) ;define

(define short-circuit-validator
  (make-json-schema-validator short-circuit-schema
    `((formats probe) (format-checkers . ,(list (cons 'probe counting-format))))
  ) ;make-json-schema-validator
) ;define
(check-false (json-schema-validator-valid? short-circuit-validator 1))
(check probe-calls => 0)

(check-report)
