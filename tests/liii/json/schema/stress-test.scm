(import (liii json schema)
  (liii json schema budget)
  (liii check)
  (scheme base)
  (scheme write)
) ;import
(check-set-mode! 'report-failed)

;; Determinism: the same validator and instance always produce the same
;; error list, in the same order, across repeated runs.

(define combo-schema
  '(("type" . "object")
    ("required" . #("id" "kind"))
    ("properties"
     ("id" ("type" . "integer"))
     ("kind" ("type" . "string"))
     ("tags" ("type" . "array") ("items" ("type" . "string"))))
    ("additionalProperties" . #f))
) ;define

(define combo-instance
  '(("id" . "x") ("kind" . 1) ("tags" . #(1 2)) ("extra" . #t))
) ;define

(define combo-validator
  (make-json-schema-validator combo-schema '((check-schema? . #f)))
) ;define

(define (combo-paths)
  (map json-schema-error-json-path
    (json-schema-validator-errors combo-validator combo-instance)
  ) ;map
) ;define

(define first-run (combo-paths))
(check-true (list? first-run))
(let loop
  ((i 0))
  (when (< i 5)
    (check (combo-paths) => first-run)
    (loop (+ i 1))
  ) ;when
) ;let

;; Interleaving validators never changes their results.

(define alpha-validator
  (make-json-schema-validator '(("type" . "integer") ("minimum" . 10))
    '((check-schema? . #f))
  ) ;make-json-schema-validator
) ;define

(define beta-validator
  (make-json-schema-validator
    '(("$ref" . "#/$defs/leaf") ("$defs" ("leaf" ("type" . "string"))))
    '((check-schema? . #f))
  ) ;make-json-schema-validator
) ;define

(define alpha-result (json-schema-validator-errors alpha-validator 5))

(define beta-result (json-schema-validator-errors beta-validator 42))
(let loop
  ((i 0))
  (when (< i 3)
    (check (json-schema-validator-errors alpha-validator 5) => alpha-result)
    (check (json-schema-validator-errors beta-validator 42) => beta-result)
    (loop (+ i 1))
  ) ;when
) ;let

;; A failed run leaves no state behind: the same validator works
;; normally after an abort, and a failing run does not poison the next
;; one.

(define self-ref '(("$ref" . "#")))

(define poisoned-validator
  (make-json-schema-validator self-ref '((check-schema? . #f)))
) ;define
(check-false (guard (condition (#t #f))
               (json-schema-validator-valid? poisoned-validator 1)
               #t
             ) ;guard
) ;check-false
(check-true (json-schema-validator-valid? alpha-validator 20))
(check-true (json-schema-validator-valid? beta-validator "x"))
(check-false (guard (condition (#t #f))
               (json-schema-validator-valid? poisoned-validator 1)
               #t
             ) ;guard
) ;check-false

;; Self recursion and mutual recursion end with the budget error.
(check-false
  (guard (condition (#t #f))
    (json-schema-valid? 1 self-ref '((check-schema? . #f)))
    #t
  ) ;guard
) ;check-false

(define mutual-schema
  '(("$ref" . "#/$defs/a")
    ("$defs" ("a" ("$ref" . "#/$defs/b")) ("b" ("$ref" . "#/$defs/a"))))
) ;define
(check-false
  (guard (condition (#t #f))
    (json-schema-valid? 1 mutual-schema '((check-schema? . #f)))
    #t
  ) ;guard
) ;check-false

;; A budget can be tightened by the caller and the behaviour changes.

(define tight-validator
  (make-json-schema-validator combo-schema
    '((check-schema? . #f) (max-depth . 1))
  ) ;make-json-schema-validator
) ;define
(check-false
  (guard (condition (#t #f))
    (json-schema-validator-valid? tight-validator '(("id" . 1) ("kind" . "k")))
    #t
  ) ;guard
) ;check-false
(check-true (json-schema-validator-valid? combo-validator '(("id" . 1)
                                                            ("kind" . "k")))
) ;check-true

;; An exponential reference graph is bounded by the step budget.

(define (exponential-schema depth)
  (let loop
    ((i 0) (defs (list (cons "n0" '(("type" . "integer"))))))
    (if (> i depth)
      defs
      (loop (+ i 1)
        (cons
          (cons (string-append "n" (number->string (+ i 1)))
            (list
              (cons "allOf"
                (vector
                  (list (cons "$ref" (string-append "#/$defs/n" (number->string i))))
                  (list (cons "$ref" (string-append "#/$defs/n" (number->string i))))
                ) ;vector
              ) ;cons
            ) ;list
          ) ;cons
          defs
        ) ;cons
      ) ;loop
    ) ;if
  ) ;let
) ;define

(define (exponential-root depth)
  (list (cons "$defs" (exponential-schema depth))
    (cons "$ref" (string-append "#/$defs/n" (number->string depth)))
  ) ;list
) ;define
(check-true (json-schema-valid? 1 (exponential-root 6) '((check-schema? . #f))))
(check-false
  (guard (condition (#t #f))
    (json-schema-valid? 1
      (exponential-root 20)
      '((check-schema? . #f) (max-steps . 1000))
    ) ;json-schema-valid?
    #t
  ) ;guard
) ;check-false

;; The reference budget bounds the number of resolutions.

(define ref-heavy
  '(("$ref" . "#/$defs/x")
    ("$defs"
     ("x" ("allOf" . #((("$ref" . "#/$defs/y")) (("$ref" . "#/$defs/y")))))
     ("y" ("type" . "integer"))))
) ;define
(check-true (json-schema-valid? 1 ref-heavy '((check-schema? . #f))))
(check-false
  (guard (condition (#t #f))
    (json-schema-valid? 1 ref-heavy '((check-schema? . #f) (max-references . 1)))
    #t
  ) ;guard
) ;check-false

;; Deeply nested instances are bounded by the depth budget.

(define (deep-instance depth)
  (if (zero? depth) '(()) (list (cons "a" (deep-instance (- depth 1)))))
) ;define

(define deep-schema '(("type" . "object") ("properties" ("a" ("$ref" . "#")))))
(check-true (json-schema-valid? '(()) deep-schema '((check-schema? . #f))))
(check-false
  (guard (condition (#t #f))
    (json-schema-valid? (deep-instance 1100) deep-schema '((check-schema? . #f)))
    #t
  ) ;guard
) ;check-false
(check-true (json-schema-valid? (deep-instance 900)
              deep-schema
              '((check-schema? . #f) (max-depth . 10000))
            ) ;json-schema-valid?
) ;check-true

;; A huge but ordinary instance stays fast and correct.

(define (wide-instance n)
  (let loop
    ((i 0) (pairs '()))
    (if (= i n)
      (reverse pairs)
      (loop (+ i 1) (cons (cons (string-append "k" (number->string i)) i) pairs))
    ) ;if
  ) ;let
) ;define

(define wide-schema
  '(("type" . "object")
    ("propertyNames" ("pattern" . "^k[0-9]+$"))
    ("additionalProperties" ("type" . "integer"))
    ("minProperties" . 1))
) ;define
(check-true (json-schema-valid? (wide-instance 500) wide-schema '((check-schema?
                                                                    . #f)))
) ;check-true
(check-catch 'json-schema-budget-exceeded
  (json-schema-valid? (wide-instance 500)
    wide-schema
    '((check-schema? . #f) (max-steps . 100))
  ) ;json-schema-valid?
) ;check-catch

;; Budget error objects are distinguishable from other conditions.

;; Budget errors are catchable by their error identifier, so callers can
;; tell them apart from an invalid schema or an unresolvable reference.
(check-catch 'json-schema-budget-exceeded
  (json-schema-valid? 1 self-ref '((check-schema? . #f)))
) ;check-catch
(check-catch 'json-schema-unresolvable
  (json-schema-valid? 1
    '(($ref . "https://missing.example/x"))
    '((check-schema? . #f))
  ) ;json-schema-valid?
) ;check-catch

(check-report)
