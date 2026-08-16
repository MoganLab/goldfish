(library (extensible-match internal-tests)
  (export run-tests)
  (import (rnrs (6))
          (only (srfi :1 lists) list=)
          (chibi test)
          (extensible-match ast)
          (extensible-match core-pattern)
          (extensible-match decision-tree))

  (define (patacts=? a b) (list= ast=? a b))
  (define-record-type (Box box box?)
    (fields (mutable content unbox box-set!)))
  (define-record-type (Better-Box better-box better-box?)
    (parent Box))

  (define (run-tests)
    ;; TODO: Testing of the AST -> AST transformations should also
    ;; automatically run the tests with the patterns to transform
    ;; embedded in a variety of other patterns

    (test-group "Pattern subject unification"
      ;; This test depends on implementation details, namely that the
      ;; first set of subjct variables for a particular application
      ;; are the ones that will be chosen for all the others.
      (test-equal
          patacts=? "Basic unification works"
          (list
           (make-patact (make-row-pattern
                         (list
                          (make-apply-pattern #'p 0 #'car
                                              (list #'a1)
                                              (make-row-pattern
                                               (list (make-var-pattern #'a1 #'x))))
                          (make-apply-pattern #'p 1 #'cdr
                                              (list #'d1)
                                              (make-row-pattern
                                               (list (make-var-pattern #'d1 #'y))))))
                        (make-action #'a (list #'x #'y)))
           (make-patact (make-row-pattern
                         (list
                          (make-apply-pattern #'p 0 #'car
                                              (list #'a1)
                                              (make-row-pattern
                                               (list (make-var-pattern #'a1 #'x))))
                          (make-apply-pattern #'p 1 #'cdr
                                              (list #'d1)
                                              (make-row-pattern
                                               (list (make-var-pattern #'d1 #'y))))))
                        (make-action #'a (list #'x #'y))))
        (patacts-unify-subjects
         (list
           (make-patact (make-row-pattern
                         (list
                          (make-apply-pattern #'p 0 #'car
                                              (list #'a1)
                                              (make-row-pattern
                                               (list (make-var-pattern #'a1 #'x))))
                          (make-apply-pattern #'p 1 #'cdr
                                              (list #'d1)
                                              (make-row-pattern
                                               (list (make-var-pattern #'d1 #'y))))))
                        (make-action #'a (list #'x #'y)))
           (make-patact (make-row-pattern
                         (list
                          (make-apply-pattern #'p 0 #'car
                                              (list #'a2)
                                              (make-row-pattern
                                               (list (make-var-pattern #'a2 #'x))))
                          (make-apply-pattern #'p 1 #'cdr
                                              (list #'d2)
                                              (make-row-pattern
                                               (list (make-var-pattern #'d2 #'y))))))
                        (make-action #'a (list #'x #'y)))))))

    (test-group "And renesting"
      (test-equal
          ast=? "Basic renesting"
          (make-and-pattern
           (make-?-pattern #'b 0 #'box?)
           (make-and-pattern
            (make-apply-pattern #'b 1 #'unbox
                                (list #'bc)
                                (make-quote-pattern #'bc 0))
            (make-?-pattern #'b 2 #'better-box?)))
        (pattern-renest-ands
         (make-and-pattern
          (make-and-pattern
           (make-?-pattern #'b 0 #'box?)
           (make-apply-pattern #'b 1 #'unbox
                               (list #'bc)
                               (make-quote-pattern #'bc 0)))
          (make-?-pattern #'b 2 #'better-box?))))
      
      (test-equal
          ast=? "Variables are moved to the end"
          (make-and-pattern
           (make-?-pattern #'b 0 #'box?)
           (make-and-pattern
            (make-apply-pattern #'b 1 #'unbox
                                (list #'bc)
                                (make-quote-pattern #'bc 0))
            (make-var-pattern #'b #'the-box)))
        (pattern-renest-ands
         (make-and-pattern
          (make-var-pattern #'b #'the-box)
          (make-and-pattern
           (make-?-pattern #'b 0 #'box?)
           (make-apply-pattern #'b 1 #'unbox
                               (list #'bc)
                               (make-quote-pattern #'bc 0))))))

      (test-equal
          ast=? "Renesting and moving variables to the end"
          (make-and-pattern
           (make-?-pattern #'b 0 #'box?)
           (make-and-pattern
            (make-apply-pattern #'b 1 #'unbox
                                (list #'bc)
                                (make-quote-pattern #'bc 0))
            (make-and-pattern
             (make-?-pattern #'b 2 #'better-box?)
             (make-var-pattern #'b #'the-box))))
        (pattern-renest-ands
         (make-and-pattern
          (make-and-pattern
           (make-var-pattern #'b #'the-box)
           (make-and-pattern
            (make-?-pattern #'b 0 #'box?)
            (make-apply-pattern #'b 1 #'unbox
                                (list #'bc)
                                (make-quote-pattern #'bc 0))))
          (make-?-pattern #'b 2 #'better-box?)))))

    (test-group "Removing vars from nots"
      (test-equal
          ast=?
          "Basic case"
          (make-not-pattern (make-wildcard-pattern #'x))
        (pattern-remove-not-vars
         (make-not-pattern (make-var-pattern #'x #'y))))

      (test-equal
          ast=?
          "In not not"
          (make-not-pattern
           (make-not-pattern (make-wildcard-pattern #'x)))
        (pattern-remove-not-vars
         (make-not-pattern
          (make-not-pattern (make-var-pattern #'x #'y))))))

    (test-group "Demorganization"
      (test-equal
          ast=?
          "Not over ands"
          (make-or-pattern
           (make-not-pattern (make-?-pattern #'x 0 #'a?))
           (make-or-pattern
            (make-not-pattern (make-?-pattern #'x 1 #'b?))
            (make-not-pattern (make-?-pattern #'x 2 #'c?))))
        (pattern-de-morgan
         (make-not-pattern
          (make-and-pattern
           (make-?-pattern #'x 0 #'a?)
           (make-and-pattern
            (make-?-pattern #'x 1 #'b?)
            (make-?-pattern #'x 2 #'c?))))))

      (test-equal
          ast=?
          "Not over ors"
          (make-and-pattern
           (make-not-pattern (make-?-pattern #'x 0 #'a?))
           (make-and-pattern
            (make-not-pattern (make-?-pattern #'x 1 #'b?))
            (make-not-pattern (make-?-pattern #'x 2 #'c?))))
        (pattern-de-morgan
         (make-not-pattern
          (make-or-pattern
           (make-?-pattern #'x 0 #'a?)
           (make-or-pattern
            (make-?-pattern #'x 1 #'b?)
            (make-?-pattern #'x 2 #'c?))))))

      (test-equal
          ast=?
          "Not over a one-pattern row"
          (make-not-pattern (make-?-pattern #'x 0 #'a?))
        (pattern-de-morgan
         (make-not-pattern (make-row-pattern
                            (list (make-?-pattern #'x 0 #'a?))))))

      (test-equal
          ast=?
          "Not over apply"
          (make-apply-pattern #'x 0 #'get-a (list #'x-a)
                              (make-not-pattern (make-wildcard-pattern #'x-a)))
        (pattern-de-morgan
         (make-not-pattern
          (make-apply-pattern #'x 0 #'get-a (list #'x-a)
                              (make-wildcard-pattern #'x-a)))))

      (test-equal
          ast=?
          "Not over not"
          (make-wildcard-pattern #'x)
        (pattern-de-morgan
         (make-not-pattern
          (make-not-pattern
           (make-wildcard-pattern #'x))))))

    (test-group "Basic decision tree generation"
      (test-assert "Selects an appropriate column to specialize (left)"
        (let ((dt
               (patacts->decision-tree
                (list
                 (make-patact
                  (make-row-pattern
                   (list
                    (make-apply-pattern #'x 0 #'car (list #'a)
                                        (make-row-pattern
                                         (list (make-quote-pattern #'a 'refutable1))))
                    (make-apply-pattern #'x 0 #'cdr (list #'d)
                                        (make-row-pattern
                                         (list (make-quote-pattern #'d 'refutable2))))))
                  (make-action #'finish1 '()))
                 (make-patact
                  (make-row-pattern
                   (list
                    (make-apply-pattern #'x 0 #'car (list #'a)
                                        (make-row-pattern
                                         (list (make-quote-pattern #'a 'refutable3))))
                    (make-apply-pattern #'x 0 #'cdr (list #'d)
                                        (make-row-pattern
                                         (list (make-wildcard-pattern #'d))))))
                  (make-action #'finish2 '()))))))
          (and (dt-apply? dt)
               (bound-identifier=? (dt-apply-var dt) #'x)
               (free-identifier=? (dt-apply-proc dt) #'car))))
      (test-assert "Selects an appropriate column to specialize (right)"
        (let ((dt
               (patacts->decision-tree
                (list
                 (make-patact
                  (make-row-pattern
                   (list
                    (make-apply-pattern #'x 0 #'car (list #'a)
                                        (make-row-pattern
                                         (list (make-wildcard-pattern #'a))))
                    (make-apply-pattern #'x 1 #'cdr (list #'d)
                                        (make-row-pattern
                                         (list (make-quote-pattern #'d 'refutable1))))))
                  (make-action #'finish1 '()))
                 (make-patact
                  (make-row-pattern
                   (list
                    (make-apply-pattern #'x 0 #'car (list #'a)
                                        (make-row-pattern
                                         (list (make-quote-pattern #'a 'refutable2))))
                    (make-apply-pattern #'x 1 #'cdr (list #'d)
                                        (make-row-pattern
                                         (list (make-quote-pattern #'d 'refutable3))))))
                  (make-action #'finish2 '()))))))
          (and (dt-apply? dt)
               (bound-identifier=? (dt-apply-var dt) #'x)
               (free-identifier=? (dt-apply-proc dt) #'cdr)))))

    (test-group "Complementary patterns"
      (define (complementary-core-patterns? a b)
        (let ((ast-pats (core-patterns->ast
                         (list #`(core:subject x #,a)
                               #`(core:subject x #,b)))))
          (and (complementary-patterns? (car ast-pats) (cadr ast-pats))
               (complementary-patterns? (cadr ast-pats) (car ast-pats)))))
      (define (noncomplementary-core-patterns? a b)
        (let ((ast-pats (core-patterns->ast
                         (list #`(core:subject x #,a)
                               #`(core:subject x #,b)))))
          (and (not (complementary-patterns? (car ast-pats) (cadr ast-pats)))
               (not (complementary-patterns? (cadr ast-pats) (car ast-pats))))))

      (test-assert "Distinct quote patterns"
        (complementary-core-patterns? #'(core:quote ())
                                      #'(core:quote #f)))
      (test-assert "Indistinct quote patterns"
        (noncomplementary-core-patterns? #'(core:quote ())
                                         #'(core:quote ())))

      (test-assert "Quote pattern with predicate for disjoint datum type"
        (complementary-core-patterns? #'(core:quote ())
                                      #'(core:? pair?)))

      (test-assert "And pattern is complementary with non-and pattern"
        (complementary-core-patterns?
         #'(core:and (core:? pair?)
                     (core:apply car (a) (core:subject a (core:quote a))))
         #'(core:quote ())))

      (test-assert "Apply patterns with complementary subpatterns are complementary"
        (complementary-core-patterns?
         #'(core:apply car (v)
                       (core:subject v (core:quote 1)))
         #'(core:apply car (v)
                       (core:subject v (core:quote 2)))))
      (test-assert "Apply patterns with identical subpatterns are not complementary"
        (noncomplementary-core-patterns?
         #'(core:apply car (v)
                       (core:subject v (core:quote 1)))
         #'(core:apply car (v)
                       (core:subject v (core:quote 1)))))
      (test-assert "Apply patterns with distinct procedures are not complementary"
        (noncomplementary-core-patterns?
         #'(core:apply car (v)
                       (core:subject v (core:quote 1)))
         #'(core:apply cdr (w)
                       (core:subject w (core:quote 2))))))))

;; Local Variables:
;; eval: (put 'test 'scheme-indent-function 2)
;; eval: (put 'test-equal 'scheme-indent-function 3)
;; eval: (put 'test-error 'scheme-indent-function 'defun)
;; eval: (put 'test-assert 'scheme-indent-function 'defun)
;; eval: (put 'test-not 'scheme-indent-function 'defun)
;; eval: (put 'test-group 'scheme-indent-function 1)
;; eval: (put 'test-values 'scheme-indent-function 2)
;; End:
