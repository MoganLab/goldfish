(library (extensible-match expand)
  (export expand-pattern
          expand-patterns
          expand-patternses)
  (import (rnrs (6))
          (extensible-match pattern-syntax)
          (extensible-match core-pattern)
          (extensible-match seq-pattern))

  ;; Expand a pattern into a core pattern and pass it on to another
  ;; macro
  (define-syntax expand-pattern
    (lambda (stx)
      (core-pattern-case stx ()
        ((_ id keyword . subforms)
         (identifier? #'id)
         (cond ((free-identifier=? #'id #'(... ...))
                (syntax-violation 'match
                                  "ellipsis in the wrong place"
                                  stx))
               ((free-identifier=? #'id #'_)
                #'(keyword (core:wildcard) . subforms))
               (else
                #'(keyword (core:var id) . subforms))))
        ((_ (core:var id) keyword . subforms)
         (identifier? #'id)
         #'(keyword (core:var id) . subforms))
        ((_ (core:wildcard) keyword . subforms)
         #'(keyword (core:wildcard) . subforms))
        ((_ (core:quote val) keyword . subforms)
         #'(keyword (core:quote val) . subforms))
        ((_ (core:and subpat_1 subpat_2)
            keyword . subforms)
         #'(expand-pattern subpat_1 expand-boolean-aux_1 core:and subpat_2
                           keyword . subforms))
        ((_ (core:or subpat_1 subpat_2)
            keyword . subforms)
         #'(expand-pattern subpat_1 expand-boolean-aux_1 core:or subpat_2
                           keyword . subforms))
        ((_ (core:row subpat ...)
            keyword . subforms)
         #'(expand-row-aux_n () (subpat ...) keyword . subforms))
        ((_ (core:subject id subpat)
            keyword . subforms)
         #'(expand-pattern subpat expand-subject-aux id keyword . subforms))
        ((_ (core:not subpat) keyword . subforms)
         #'(expand-pattern subpat expand-not-aux keyword . subforms))
        ((_ (core:? pred)
            keyword . subforms)
         #'(keyword (core:? pred) . subforms))
        ((_ (core:apply proc (vars ...) subpat)
            keyword . subforms)
         #'(expand-pattern subpat
                           expand-apply-aux proc (vars ...)
                           keyword . subforms))
        ((_ (core:seq type name state-vars terminate? ref seq-pat ...)
                           keyword . subforms)
         #'(expand-seq-aux_n () (seq-pat ...)
                             (type name state-vars terminate? ref)
                             keyword . subforms))
        ((_ null keyword . subforms)
         (null? (syntax->datum #'null))
         (syntax-violation 'match
                           "empty list used as a pattern"
                           #'null))
        ((_ (invalid . more)
            keyword . subforms)
         (not (identifier? #'invalid))
         (syntax-violation 'match
                           "invalid subpattern form"
                           #'(invalid . more)))
        ((_ (derived-operator . operands)
            keyword . subforms)
         #'(expand-pattern-syntax (derived-operator . operands)
                                  expand-pattern keyword . subforms))
        ((_ constant keyword . subforms)
         #'(keyword (core:quote constant) . subforms)))))


  ;; Auxiliary macros used while expanding a macro into a core
  ;; pattern
  (define-syntax expand-seq-aux_n
    (syntax-rules ()
      ((_ (expanded ...) () (meta ...) keyword . subforms)
       (keyword (core:seq meta ... expanded ...) . subforms))
      ((_ (expanded ...) (subpat more ...) meta keyword . subforms)
       (expand-seq-pattern subpat expand-seq-aux_n-append (expanded ...) (more ...) meta keyword . subforms))))
  (define-syntax expand-seq-aux_n-append
    (syntax-rules ()
      ((_ expanded_1 (expanded_n ...) (more ...) meta keyword . subforms)
       (expand-seq-aux_n (expanded_n ... expanded_1) (more ...) meta keyword . subforms))))

  (define-syntax expand-seq-pattern
    (syntax-rules (core/seq:one core/seq:many)
      ((_ (core/seq:one subpat) keyword . subpats)
       (expand-pattern subpat expand-seq-pattern-aux (core/seq:one) keyword . subpats))
      ((_ (core/seq:many min max subpat) keyword . subpats)
       (expand-pattern subpat expand-seq-pattern-aux (core/seq:many min max) keyword . subpats))))
  (define-syntax expand-seq-pattern-aux
    (syntax-rules ()
      ((_ expanded (operator operand ...) keyword . subforms)
       (keyword (operator operand ... expanded) . subforms))))

  (define-syntax expand-boolean-aux_1
    (syntax-rules ()
      ((_ bool-op expanded_1 subpat_2
          keyword . subforms)
       (expand-pattern subpat_2 expand-boolean-aux_2 bool-op expanded_1
                       keyword . subforms))))
  (define-syntax expand-boolean-aux_2
    (syntax-rules ()
      ((_ expanded_2 expanded_1 bool-op
          keyword . subforms)
       (keyword (bool-op expanded_1 expanded_2) . subforms))))

  (define-syntax expand-not-aux
    (syntax-rules ()
      ((_ expanded keyword . subforms)
       (keyword (core:not expanded) . subforms))))

  (define-syntax expand-row-aux_n
    (syntax-rules ()
      ((_ (expanded ...) () keyword . subforms)
       (keyword (core:row expanded ...) . subforms))
      ((_ (expanded ...) (subpat more-subpats ...) keyword . subforms)
       (expand-pattern subpat expand-row-aux_1
                       (expanded ...) (more-subpats ...)
                       keyword . subforms))))
  (define-syntax expand-row-aux_1
    (syntax-rules ()
      ((_ expanded (more-expanded ...) (more-subpats ...)
          keyword . subforms)
       (expand-row-aux_n (more-expanded ... expanded) (more-subpats ...)
                         keyword . subforms))))

  (define-syntax expand-subject-aux
    (syntax-rules ()
      ((_ expanded id keyword . subforms)
       (keyword (core:subject id expanded) . subforms))))

  (define-syntax expand-apply-aux
    (syntax-rules ()
      ((_ expanded proc (var ...) keyword . subform)
       (keyword (core:apply proc (var ...) expanded) . subform))))

  ;; Expand multiple patterns into their core forms and pass them all
  ;; along to another macro
  (define-syntax expand-patterns
    (syntax-rules ()
      ((_ (pat ...) keyword . subforms)
       (expand-patterns-aux_n (pat ...) () keyword . subforms))))

  (define-syntax expand-patterns-aux_n
    (syntax-rules ()
      ((_ () (expanded-pats ...) keyword . subforms)
       (keyword (expanded-pats ...) . subforms))
      ((_ (pat . more-pats) expanded-pats keyword . subforms)
       (expand-pattern pat expand-patterns-aux_1 expanded-pats more-pats
                       keyword . subforms))))
  (define-syntax expand-patterns-aux_1
    (syntax-rules ()
      ((_ expanded (expanded-pats ...) more-pats keyword . subforms)
       (expand-patterns-aux_n more-pats (expanded-pats ... expanded)
                              keyword . subforms))))

  ;; Expand lists of lists of patterns into their core forms and pass
  ;; along to another macro
  (define-syntax expand-patternses
    (syntax-rules ()
      ((_ ((pat ...) ...) keyword . subforms)
       (expand-patternses-aux_n ((pat ...) ...) () keyword . subforms))))

  (define-syntax expand-patternses-aux_n
    (syntax-rules ()
      ((_ () ((expanded-pat ...) ...) keyword . subforms)
       (keyword ((expanded-pat ...) ...) . subforms))
      ((_ (pats . more-patses) expanded-patses keyword . subforms)
       (expand-patterns pats expand-patternses-aux_1 expanded-patses more-patses
                        keyword . subforms))))
  (define-syntax expand-patternses-aux_1
    (syntax-rules ()
      ((_ expanded (expanded-patses ...) more-patses keyword . subforms)
       (expand-patternses-aux_n more-patses (expanded-patses ... expanded)
                                keyword . subforms)))))
