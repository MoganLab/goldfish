(import (liii json schema)
  (liii json schema core)
  (liii json schema evaluated)
  (liii check)
  (liii list)
  (scheme base)
) ;import
(check-set-mode! 'report-failed)

(define validator (make-json-schema-validator #t))

(define (valid? instance schema root)
  (json-schema-node-valid? validator instance schema root)
) ;define

(define (props instance schema)
  (evaluated-properties validator instance schema schema valid?)
) ;define

(define (insert-number value sorted)
  (cond ((null? sorted) (list value))
        ((< value (car sorted)) (cons value sorted))
        (else (cons (car sorted) (insert-number value (cdr sorted))))
  ) ;cond
) ;define

(define (sorted-numbers values)
  (let loop
    ((rest values) (result '()))
    (if (null? rest) result (loop (cdr rest) (insert-number (car rest) result)))
  ) ;let
) ;define

(define (items instance schema)
  (sorted-numbers (evaluated-items validator instance schema schema valid?))
) ;define

(define (insert-string value sorted)
  (cond ((null? sorted) (list value))
        ((string<? value (car sorted)) (cons value sorted))
        (else (cons (car sorted) (insert-string value (cdr sorted))))
  ) ;cond
) ;define

(define (sorted-strings values)
  (let loop
    ((rest values) (result '()))
    (if (null? rest) result (loop (cdr rest) (insert-string (car rest) result)))
  ) ;let
) ;define

;; Direct object applicators and successful allOf branches.
(check
  (sorted-strings
    (props '(("a" . 1) ("b" . 2)) '(("properties" ("a" . #t))))
  ) ;sorted-strings
  =>
  '("a")
) ;check
(check
  (sorted-strings (props '(("a" . 1) ("b" . 2))
                    '(("allOf"
                       . #((("properties" ("a" . #t)))
                           (("properties" ("b" . #t)))
                         ) ;#
                      ))
                  ) ;props
  ) ;sorted-strings
  =>
  '("a" "b")
) ;check
(check
  (sorted-strings
    (props '(("ax" . 1)) '(("patternProperties" ("^a" . #t))))
  ) ;sorted-strings
  =>
  '("ax")
) ;check

;; Failed anyOf/oneOf/if branches do not contribute their properties.
(check (props '(("b" . 1))
         '(("anyOf"
            . #((("properties" ("a" . #t)) ("required" . #("a")))
                (("properties" ("b" . #t)))
              ) ;#
           ))
       ) ;props
  =>
  '("b")
) ;check
(check (props '(("b" . 1))
         '(("oneOf"
            . #((("properties" ("a" . #t)) ("required" . #("a")))
                (("properties" ("b" . #t)))
              ) ;#
           ))
       ) ;props
  =>
  '("b")
) ;check
(check
  (sorted-strings
    (props '(("b" . 1) ("condition" . 0))
      '(("if" ("properties" ("condition" . #t)) ("required" . #("missing")))
        ("then" ("properties" ("a" . #t)))
        ("else" ("properties" ("b" . #t))))
    ) ;props
  ) ;sorted-strings
  =>
  '("b")
) ;check

;; References collect from the target resource.
(check
  (props '(("x" . 1))
    '(("$defs" ("target" ("properties" ("x" . #t))))
      ("$ref" . "#/$defs/target"))
  ) ;props
  =>
  '("x")
) ;check

;; Array prefix/items/contains and successful branches propagate indices.
(check (items #(1 2 3) '(("prefixItems" . #(#t)) ("items" . #t))) => '(0 1 2))
(check
  (items #(1 "x" 2) '(("contains" ("type" . "integer"))))
  =>
  '(0 2)
) ;check
(check (items #(1 2)
         '(("anyOf" . #((("prefixItems" . #(#f))) (("prefixItems" . #(#t #t))))))
       ) ;items
  =>
  '(0 1)
) ;check

;; unevaluatedProperties sees only properties not covered by successful applicators.
(check-true
  (json-schema-valid? '(("a" . 1))
    '(("properties" ("a" ("type" . "integer"))) ("unevaluatedProperties" . #f))
  ) ;json-schema-valid?
) ;check-true
(check-false
  (json-schema-valid? '(("a" . 1) ("b" . 2))
    '(("properties" ("a" ("type" . "integer"))) ("unevaluatedProperties" . #f))
  ) ;json-schema-valid?
) ;check-false
(check-true
  (json-schema-valid? '(("a" . 1) ("b" . "ok"))
    '(("properties" ("a" ("type" . "integer")))
      ("unevaluatedProperties" ("type" . "string")))
  ) ;json-schema-valid?
) ;check-true
(check-true (json-schema-valid? '(("a" . 1))
              '(("allOf" . #((("properties" ("a" . #t)))))
                ("unevaluatedProperties" . #f))
            ) ;json-schema-valid?
) ;check-true
(check-false (json-schema-valid? '(("a" . 1) ("b" . 2))
               '(("anyOf"
                  . #((("properties" ("a" . #t)) ("required" . #("missing")))))
                 ("unevaluatedProperties" . #f))
             ) ;json-schema-valid?
) ;check-false
(check-true (json-schema-valid? '(("b" . 1))
              '(("anyOf"
                 . #((("properties" ("a" . #t)) ("required" . #("a")))
                     (("properties" ("b" . #t)))
                   ) ;#
                )
                ("unevaluatedProperties" . #f))
            ) ;json-schema-valid?
) ;check-true

(define unevaluated-ref-schema
  '(("$defs" ("target" ("properties" ("x" . #t))))
    ("$ref" . "#/$defs/target")
    ("unevaluatedProperties" . #f))
) ;define
(check-true (json-schema-valid? '(("x" . 1)) unevaluated-ref-schema))
(check-true (json-schema-valid? 1 '(("unevaluatedProperties" . #f))))

;; unevaluatedItems filters only indices not covered by successful applicators.
(check-true (json-schema-valid? #(1)
              '(("prefixItems" . #((("type" . "integer"))))
                ("unevaluatedItems" . #f))
            ) ;json-schema-valid?
) ;check-true
(check-false (json-schema-valid? #(1 2)
               '(("prefixItems" . #((("type" . "integer"))))
                 ("unevaluatedItems" . #f))
             ) ;json-schema-valid?
) ;check-false
(check-true
  (json-schema-valid? #(1 2)
    '(("prefixItems" . #((("type" . "integer"))))
      ("items" ("type" . "integer"))
      ("unevaluatedItems" . #f))
  ) ;json-schema-valid?
) ;check-true
(check-false
  (json-schema-valid? #(1 "x")
    '(("items" ("type" . "integer")) ("unevaluatedItems" . #f))
  ) ;json-schema-valid?
) ;check-false
(check-false
  (json-schema-valid? #(1 "x")
    '(("contains" ("type" . "integer")) ("unevaluatedItems" . #f))
  ) ;json-schema-valid?
) ;check-false
(check-true (json-schema-valid? #(1)
              '(("allOf" . #((("prefixItems" . #((("type" . "integer")))))))
                ("unevaluatedItems" . #f))
            ) ;json-schema-valid?
) ;check-true
(check-true (json-schema-valid? 1 '(("unevaluatedItems" . #f))))
(let ((error
        (car (json-schema-errors #(1 2) '(("prefixItems" . #(#t))
                                          ("unevaluatedItems" . #f)))
        ) ;car
      ) ;error
     ) ;
  (check (json-schema-error-path error) => '(1))
) ;let

(check-report)
