(import (liii json schema equal) (liii json schema) (liii check) (scheme base))
(check-set-mode! 'report-failed)

;; Scalar equality follows JSON, not Scheme's boolean/numeric equivalence.
(check-true (json-equal? 1 1.0))
(check-false (json-equal? #t 1))
(check-false (json-equal? #f 0))
(check-true (json-equal? 'null 'null))
(check-false (json-equal? 'null '()))
(check-true (json-equal? "x" "x"))
(check-false (json-equal? "x" 'x))

;; Objects are unordered maps; arrays remain ordered recursive sequences.
(check-true (json-equal? '(("a" . 1) ("b" . 2)) '(("b" . 2) ("a" . 1))))
(check-false (json-equal? '(("a" . 1)) '(("a" . #t))))
(check-true (json-equal? #(1 #(2 3)) #(1.0 #(2 3))))
(check-false (json-equal? #(1 2) #(2 1)))
(check-false (json-equal? #(#t 1) #(1 #t)))
(check-true (json-equal? '(("x" . #(#t 1))) '(("x" . #(#t 1)))))
(check-false (json-equal? '(("x" . #(#t 1))) '(("x" . #(1 #t)))))

;; const and enum use exactly the same recursive equality.
(check-true (json-schema-valid? 1 '(("const" . 1.0))))
(check-false (json-schema-valid? #t '(("const" . 1))))
(check-true
  (json-schema-valid? '(("a" . 1) ("b" . 2)) '(("const" ("b" . 2) ("a" . 1))))
) ;check-true
(check-true (json-schema-valid? #(#t 1) '(("enum" . #(#(1 #t) #(#t 1))))))
(check-false (json-schema-valid? #t '(("enum" . #(0 1 null)))))

;; uniqueItems distinguishes nested booleans and numbers as well.
(check-true (json-schema-valid? #(#t 1 #f 0) '(("uniqueItems" . #t))))
(check-false (json-schema-valid? #(1 1.0) '(("uniqueItems" . #t))))
(check-true (json-schema-valid? #((("x" . #t)) (("x" . 1))) '(("uniqueItems"
                                                               . #t)))
) ;check-true
(check-false (json-schema-valid? #((("a" . 1) ("b" . 2)) (("b" . 2) ("a" . 1)))
               '(("uniqueItems" . #t))
             ) ;json-schema-valid?
) ;check-false
(check-true (json-schema-valid? #(1 1) '(("uniqueItems" . #f))))
(check-report)
