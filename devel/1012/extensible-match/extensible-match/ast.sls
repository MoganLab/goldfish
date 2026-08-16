(library (extensible-match ast)
  (export core-patterns+actions->ast
          core-patterns->ast
          action-syntax->ast
          pattern?
          pattern-subject
          make-wildcard-pattern
          wildcard-pattern?
          make-var-pattern
          var-pattern?
          var-pattern-name
          make-quote-pattern
          quote-pattern?
          quote-pattern-datum
          make-and-pattern
          and-pattern?
          and-pattern-subpat_1
          and-pattern-subpat_2
          make-or-pattern
          or-pattern?
          or-pattern-subpat_1
          or-pattern-subpat_2
          make-row-pattern
          row-pattern?
          row-pattern-subpats
          make-not-pattern
          not-pattern?
          not-pattern-subpat
          make-?-pattern
          ?-pattern?
          ?-pattern-predicate-id
          ?-pattern-predicate-expr
          make-apply-pattern
          apply-pattern?
          apply-pattern-procedure-id
          apply-pattern-procedure-expr
          apply-pattern-vars
          apply-pattern-subpat
          make-seq-pattern
          seq-pattern?
          seq-pattern-subpat-subject
          seq-pattern-type
          seq-pattern-name
          seq-pattern-state-vars
          seq-pattern-termination-expr
          seq-pattern-ref-expr
          seq-pattern-subpats

          make-seq-state-var
          seq-state-var-name
          seq-state-var-init
          seq-state-var-step

          make-seq-pattern-one
          seq-pattern-one?
          seq-pattern-one-subpat
          make-seq-pattern-many
          seq-pattern-many?
          seq-pattern-many-min
          seq-pattern-many-max
          seq-pattern-many-subpat

          make-action
          action?
          action-procedure
          action-args
          failure-action

          make-patact
          patact?
          patact-pattern
          patact-action

          ast=?
          ast-hash
          ast-cond

          patacts-unify-subjects
          pattern-unify-subjects
          pattern-rename-subjects

          patacts-renest-ands
          pattern-renest-ands

          patacts-remove-not-vars
          pattern-remove-not-vars
          
          patacts-de-morgan
          pattern-de-morgan

          patacts-source-transform

          pattern-vars
          pattern-disjointed-vars
          seq-pattern-vars

          raise-if-non-linear)
  (import (except (rnrs (6)) assoc member)
          (only (srfi :1 lists)
                append-map
                assoc
                delete-duplicates
                list=
                lset= lset-union lset-intersection lset-difference
                member
                reduce-right)
          (extensible-match util)
          (extensible-match core-pattern))

  ;; The AST layer of pattern representation is, strictly speaking,
  ;; redundant: it is a very close mirror of core patterns. However,
  ;; using it brings two advantages:
  ;;
  ;; (1) The major one is performance of the decision-tree generator.
  ;;     Testing on one moderately complex set of patterns during
  ;;     development showed that using an AST made of records instead
  ;;     of core-patterns represented as Scheme syntax objects led to
  ;;     nearly three orders of magnitude improvement in performance
  ;;     due to not having to unwrap and destructure and use the
  ;;     (fairly expensive) free-identifier=? predicate for type
  ;;     testing. When using record types, field access and type
  ;;     testing are both fast constant-time operations with no
  ;;     consing, but linear-time scans and consing (of unwrapped
  ;;     versions of originally-wrapped syntax object forms) are
  ;;     needed for the corresponding actions on syntax objects. Since
  ;;     building the decision tree has exponential complexity, this
  ;;     kind of thing really adds up in the overall performance!
  ;;
  ;;     Avoiding excessive free-identifier=? procedure calls during
  ;;     the exponential-complexity decision tree generation procedure
  ;;     is also why expressions which are part of patterns are
  ;;     interned into a table during AST construction, which makes
  ;;     compilation overall faster even though building that table
  ;;     takes quadratic time due to Scheme’s lack of a
  ;;     free-identifier-hash procedure.
  ;;
  ;; (2) A more minor benefit is that the core:subject pattern is
  ;;     eliminated, and every individual subpattern knows directly
  ;;     which identifier refers to its subject. This reduces implicit
  ;;     state in the decision-tree generator, meaning fewer chances
  ;;     of making mistakes in keeping track of state in that more
  ;;     complex algorithm, by dealing with it during the much simpler
  ;;     conversion from core patterns to the AST.
  ;;
  ;; The main disadvantage is that it reduces the ability to use one
  ;; pattern matcher (syntax-case) in the implementation of another,
  ;; but this has turned out to be less of an annoyance than I thought
  ;; it would be, except for making code longer than it would
  ;; otherwise need to be.

  (define-record-type primitive-pattern)
  (define-record-type pattern
    (fields subject)
    (parent primitive-pattern))
  (define-record-type wildcard-pattern
    (parent pattern))
  (define-record-type var-pattern
    (fields name)
    (parent pattern))
  (define-record-type quote-pattern
    (fields datum)
    (parent pattern))
  (define-record-type and-pattern
    (fields subpat_1 subpat_2)
    (parent primitive-pattern))
  (define-record-type or-pattern
    (fields subpat_1 subpat_2)
    (parent primitive-pattern))
  (define-record-type row-pattern
    (fields subpats)
    (parent primitive-pattern))
  (define-record-type not-pattern
    (fields subpat)
    (parent primitive-pattern))
  (define-record-type ?-pattern
    (fields predicate-id predicate-expr)
    (parent pattern))
  (define-record-type apply-pattern
    (fields procedure-id procedure-expr vars subpat)
    (parent pattern))

  (define-record-type seq-pattern
    (fields subpat-subject type
            name state-vars
            termination-expr ref-expr
            subpats)
    (parent pattern))

  (define-record-type seq-state-var
    (fields name init step))

  (define-record-type seq-pattern-one
    (fields subpat)
    (parent pattern))
  (define-record-type seq-pattern-many
    (fields min max subpat)
    (parent pattern))

  (define-record-type action
    (fields procedure args))
  (define failure-action (make-action #'fail '()))

  (define-record-type patact
    (fields pattern action))

  (define (core-patterns+actions->ast core-pats+actions)
    (let ((expr-table (make-hashtable values fx=?)))
      (core-pattern-case core-pats+actions ()
        (((core-pat . action) ...)
         (map (lambda (core-pat action)
                (make-patact (core-pattern->ast* core-pat expr-table #f)
                             (action-syntax->ast action)))
              #'(core-pat ...)
              #'(action ...))))))

  (define (core-patterns->ast core-pats)
    (let ((expr-table (make-hashtable values fx=?)))
      (map (lambda (core-pat)
             (core-pattern->ast* core-pat expr-table #f))
           core-pats)))

  (define (core-pattern->ast* core-pat expr-table subject)
    (core-pattern-case core-pat ()
      ((core:var id)
       (identifier? #'id)
       (begin
         (assert subject)
         (make-var-pattern subject #'id)))
      ((core:wildcard)
       (identifier? #'id)
       (begin
         (assert subject)
         (make-wildcard-pattern subject)))
      ((core:quote datum)
       (begin
         (assert subject)
         (make-quote-pattern subject (syntax->datum #'datum))))
      ((core:and subpat_1 subpat_2)
       (make-and-pattern (core-pattern->ast* #'subpat_1 expr-table subject)
                         (core-pattern->ast* #'subpat_2 expr-table subject)))
      ((core:or subpat_1 subpat_2)
       (make-or-pattern (core-pattern->ast* #'subpat_1 expr-table subject)
                        (core-pattern->ast* #'subpat_2 expr-table subject)))
      ((core:row subpat ...)
       (make-row-pattern
        (map (lambda (subpat) (core-pattern->ast* subpat expr-table subject))
             #'(subpat ...))))
      ((core:subject id subpat)
       (core-pattern->ast* #'subpat expr-table #'id))
      ((core:not subpat)
       (make-not-pattern (core-pattern->ast* #'subpat expr-table subject)))
      ((core:? pred)
       (begin
         (assert subject)
         (let ((expr-id (expr-intern! expr-table #'pred)))
           (make-?-pattern subject
                           expr-id
                           #'pred))))
      ((core:apply proc (var ...) subpat)
       (begin
         (assert subject)
         (let ((expr-id (expr-intern! expr-table #'proc)))
           (make-apply-pattern subject
                               expr-id
                               #'proc
                               #'(var ...)
                               (core-pattern->ast* #'subpat
                                                   expr-table
                                                   subject)))))
      ((core:seq seq-type name ((var init step) ...) terminate? ref seq-pattern ...)
       (let* ((subpat-subject (generate-identifier))
              (seq-subpats (map (lambda (seq-pattern)
                                  (core-seq-pattern->ast seq-pattern
                                                         expr-table
                                                         subpat-subject))
                                #'(seq-pattern ...))))
         (assert subject)
         (make-seq-pattern subject
                           subpat-subject
                           (syntax-case #'seq-type (core/seq:ordered
                                                    core/seq:partial
                                                    core/seq:unordered)
                             (core/seq:ordered 'ordered)
                             (core/seq:partial 'partial)
                             (core/seq:unordered 'unordered))
                           #'name
                           (map (lambda (args)
                                  (apply make-seq-state-var args))
                                #'((var init step) ...))
                           #'terminate?
                           #'ref
                           seq-subpats)))))

  (define (core-seq-pattern->ast seq-pattern expr-table subject)
    (core-pattern-case seq-pattern ()
      ((core/seq:one pat)
       (make-seq-pattern-one subject
                             (core-pattern->ast* #'pat expr-table subject)))
      ((core/seq:many min max pat)
       (make-seq-pattern-many subject
                              (syntax->datum #'min)
                              (syntax->datum #'max)
                              (core-pattern->ast* #'pat expr-table subject)))))

  (define (expr-intern! expr-table expr)
    (cond ((identifier? expr)
           (let-values (((ks vs) (hashtable-entries expr-table)))
             (let loop ((idx 0))
               (cond ((>= idx (vector-length ks))
                      (hashtable-set! expr-table (vector-length ks) expr)
                      (vector-length ks))
                     ((and (identifier? (vector-ref vs idx))
                           (free-identifier=? expr
                                              (vector-ref vs idx)))
                      (vector-ref ks idx))
                     (else
                      (loop (+ idx 1)))))))
          (else
           (let ((new-id (hashtable-size expr-table)))
             (hashtable-set! expr-table new-id expr)
             new-id))))

  (define (action-syntax->ast act)
    (syntax-case act ()
      ((proc arg ...)
       (make-action #'proc #'(arg ...)))))

  (define (ast=? ast_1 ast_2)
    (assert (and (or (patact? ast_1)
                     (primitive-pattern? ast_1)
                     (action? ast_1))
                 (or (patact? ast_2)
                     (primitive-pattern? ast_2)
                     (action? ast_2))))
    (or (eq? ast_1 ast_2)
        (and (patact? ast_1)
             (patact? ast_2)
             (ast=? (patact-pattern ast_1)
                    (patact-pattern ast_2))
             (ast=? (patact-action ast_1)
                    (patact-action ast_2)))
        (and (primitive-pattern? ast_1)
             (primitive-pattern? ast_2)
             (or (and (pattern? ast_1)
                      (pattern? ast_2)
                      (bound-identifier=? (pattern-subject ast_1)
                                          (pattern-subject ast_2))
                      (or (and (wildcard-pattern? ast_1)
                               (wildcard-pattern? ast_2))
                          (and (var-pattern? ast_1)
                               (var-pattern? ast_2)
                               (bound-identifier=? (var-pattern-name ast_1)
                                                   (var-pattern-name ast_2)))
                          (and (quote-pattern? ast_1)
                               (quote-pattern? ast_2)
                               (equal? (quote-pattern-datum ast_1)
                                       (quote-pattern-datum ast_2)))
                          (and (?-pattern? ast_1)
                               (?-pattern? ast_2)
                               (fx=? (?-pattern-predicate-id ast_1)
                                     (?-pattern-predicate-id ast_2)))
                          (and (apply-pattern? ast_1)
                               (apply-pattern? ast_2)
                               (fx=? (apply-pattern-procedure-id ast_1)
                                     (apply-pattern-procedure-id ast_2))
                               (list= bound-identifier=?
                                      (apply-pattern-vars ast_1)
                                      (apply-pattern-vars ast_2))
                               (ast=? (apply-pattern-subpat ast_1)
                                      (apply-pattern-subpat ast_2)))
                          (and (seq-pattern? ast_1)
                               (seq-pattern? ast_2)
                               (eqv? ast_1 ast_2))))
                 (and (and-pattern? ast_1)
                      (and-pattern? ast_2)
                      (ast=? (and-pattern-subpat_1 ast_1)
                             (and-pattern-subpat_1 ast_2))
                      (ast=? (and-pattern-subpat_2 ast_1)
                             (and-pattern-subpat_2 ast_2)))
                 (and (or-pattern? ast_1)
                      (or-pattern? ast_2)
                      (ast=? (or-pattern-subpat_1 ast_1)
                             (or-pattern-subpat_1 ast_2))
                      (ast=? (or-pattern-subpat_2 ast_1)
                             (or-pattern-subpat_2 ast_2)))
                 (and (row-pattern? ast_1)
                      (row-pattern? ast_2)
                      (lset= ast=?
                             (row-pattern-subpats ast_1)
                             (row-pattern-subpats ast_2)))
                 (and (not-pattern? ast_1)
                      (not-pattern? ast_2)
                      (ast=? (not-pattern-subpat ast_1)
                             (not-pattern-subpat ast_2)))))
        (and (action? ast_1)
             (action? ast_2)
             (bound-identifier=? (action-procedure ast_1)
                                 (action-procedure ast_2))
             (list= bound-identifier=?
                    (action-args ast_1)
                    (action-args ast_2)))))

  (define (ast-hash ast)
    (cond ((patact? ast)
           (hash-combine (ast-hash (patact-pattern ast))
                         (datum-hash (syntax->datum (patact-action ast)))))
          ((pattern? ast)
           (hash-combine
            (bound-identifier-hash (pattern-subject ast))
            (cond ((wildcard-pattern? ast)
                   ;; chosen by random dice roll, guaranteed to be fair
                   #x31C668)
                  ((var-pattern? ast)
                   (bound-identifier-hash (var-pattern-name ast)))
                  ((quote-pattern? ast)
                   (datum-hash (quote-pattern-datum ast)))
                  ((?-pattern? ast)
                   (datum-hash (?-pattern-predicate-id ast)))
                  ((apply-pattern? ast)
                   (hash-combine
                    (datum-hash (apply-pattern-procedure-id ast))
                    (fold-left bitwise-xor
                               0
                               (map bound-identifier-hash
                                    (apply-pattern-vars ast)))
                    (ast-hash (apply-pattern-subpat ast))))
                  ((seq-pattern? ast)
                   (hash-combine #x13C829
                                 (bound-identifier-hash
                                  (seq-pattern-subpat-subject ast))
                                 (bound-identifier-hash
                                  (seq-pattern-name ast)))))))
          ((and-pattern? ast)
           (hash-combine #x27B7D7
                         (ast-hash (and-pattern-subpat_1 ast))
                         (ast-hash (and-pattern-subpat_2 ast))))
          ((or-pattern? ast)
           (hash-combine #x34EB53
                         (ast-hash (or-pattern-subpat_1 ast))
                         (ast-hash (or-pattern-subpat_2 ast))))
          ((row-pattern? ast)
           (hash-combine #x2C7A72
                         (fold-left bitwise-xor
                                    0
                                    (map ast-hash
                                         (row-pattern-subpats ast)))))
          ((not-pattern? ast)
           (hash-combine #x41FAC0
                         (ast-hash (not-pattern-subpat ast))))))

  (define-syntax ast-cond
    (syntax-rules ()
      ((_ (how-expr what-expr) clause ...)
       (let ((what what-expr) (how how-expr))
         (cond clause ...
               ((and-pattern? what)
                (make-and-pattern (how (and-pattern-subpat_1 what))
                                  (how (and-pattern-subpat_2 what))))
               ((or-pattern? what)
                (make-or-pattern (how (or-pattern-subpat_1 what))
                                 (how (or-pattern-subpat_2 what))))
               ((row-pattern? what)
                (make-row-pattern (map how (row-pattern-subpats what))))
               ((not-pattern? what)
                (make-not-pattern (how (not-pattern-subpat what))))
               ((apply-pattern? what)
                (make-apply-pattern (pattern-subject what)
                                    (apply-pattern-procedure-id what)
                                    (apply-pattern-procedure-expr what)
                                    (apply-pattern-vars what)
                                    (how (apply-pattern-subpat what))))
               ((seq-pattern? what)
                (make-seq-pattern (pattern-subject what)
                                  (seq-pattern-subpat-subject what)
                                  (seq-pattern-type what)
                                  (seq-pattern-name what)
                                  (seq-pattern-state-vars what)
                                  (seq-pattern-termination-expr what)
                                  (seq-pattern-ref-expr what)
                                  (map
                                   (lambda (seq-subpat)
                                     (cond ((seq-pattern-one? seq-subpat)
                                            (make-seq-pattern-one
                                             (pattern-subject seq-subpat)
                                             (how (seq-pattern-one-subpat seq-subpat))))
                                           ((seq-pattern-many? seq-subpat)
                                            (make-seq-pattern-many
                                             (pattern-subject seq-subpat)
                                             (seq-pattern-many-min seq-subpat)
                                             (seq-pattern-many-max seq-subpat)
                                             (how (seq-pattern-many-subpat seq-subpat))))))
                                   (seq-pattern-subpats what))))
               (else what))))))

  ;; Source-level optimizations and processing on ASTs

  (define (patacts-pattern-transformation proc)
    (lambda (patacts)
      (map (lambda (patact)
             (make-patact (proc (patact-pattern patact))
                          (patact-action patact)))
           patacts)))

  (define (patacts-source-transform patacts)
    (patacts-renest-ands
     (patacts-de-morgan
      (patacts-remove-not-vars
       (patacts-unify-subjects patacts)))))

  ;; Subject unification ensures that applications of the same
  ;; procedure to the same subject yield same-named subject variables
  ;; in all patterns
  (define-record-type application-template
    (fields subject procedure-id n-args))
  (define (application-template=? at_1 at_2)
    (and (bound-identifier=? (application-template-subject at_1)
                             (application-template-subject at_2))
         (fx=? (application-template-procedure-id at_1)
               (application-template-procedure-id at_2))
         (fx=? (application-template-n-args at_1)
               (application-template-n-args at_2))))
  (define (application-template-hash at)
    (hash-combine (bound-identifier-hash (application-template-subject at))
                  (datum-hash (application-template-procedure-id at))
                  (datum-hash (application-template-n-args at))))
  (define (apply-pattern->application-template pattern)
    (make-application-template (pattern-subject pattern)
                               (apply-pattern-procedure-id pattern)
                               (length (apply-pattern-vars pattern))))

  (define (patacts-unify-subjects patacts)
    (let ((ht (make-hashtable application-template-hash
                              application-template=?)))
      (map (lambda (patact)
             (make-patact
              (pattern-unify-subjects (patact-pattern patact)
                                      ht)
              (patact-action patact)))
           patacts)))

  (define (pattern-unify-subjects pattern ht)
    (ast-cond ((lambda (subpat)
                 (pattern-unify-subjects subpat ht))
               pattern)
      ((apply-pattern? pattern)
       (let ((at (apply-pattern->application-template pattern)))
         (cond ((hashtable-ref ht at #f)
                => (lambda (new-vars)
                     (make-apply-pattern
                      (pattern-subject pattern)
                      (apply-pattern-procedure-id pattern)
                      (apply-pattern-procedure-expr pattern)
                      new-vars
                      (pattern-unify-subjects
                       (pattern-rename-subjects
                        (apply-pattern-subpat pattern)
                        (map cons
                             (apply-pattern-vars pattern)
                             new-vars))
                       ht))))
               (else
                (hashtable-set! ht
                                at
                                (apply-pattern-vars pattern))
                (make-apply-pattern
                 (pattern-subject pattern)
                 (apply-pattern-procedure-id pattern)
                 (apply-pattern-procedure-expr pattern)
                 (apply-pattern-vars pattern)
                 (pattern-unify-subjects (apply-pattern-subpat pattern)
                                         ht))))))))

  (define (pattern-rename-subjects pattern renames)
    (ast-cond ((lambda (subpat)
                 (pattern-rename-subjects subpat renames))
               pattern)
      ((and (pattern? pattern)
            (assoc (pattern-subject pattern) renames bound-identifier=?))
       => (lambda (rename)
            (cond ((wildcard-pattern? pattern)
                   (make-wildcard-pattern (cdr rename)))
                  ((var-pattern? pattern)
                   (make-var-pattern (cdr rename)
                                     (var-pattern-name pattern)))
                  ((quote-pattern? pattern)
                   (make-quote-pattern (cdr rename)
                                       (quote-pattern-datum pattern)))
                  ((?-pattern? pattern)
                   (make-?-pattern (cdr rename)
                                   (?-pattern-predicate-id pattern)
                                   (?-pattern-predicate-expr pattern)))
                  ((apply-pattern? pattern)
                   (make-apply-pattern (cdr rename)
                                       (apply-pattern-procedure-id pattern)
                                       (apply-pattern-procedure-expr pattern)
                                       (apply-pattern-vars pattern)
                                       (pattern-rename-subjects
                                        (apply-pattern-subpat pattern)
                                        renames)))
                  ((seq-pattern? pattern)
                   ;; Assumption: No subpattern of the seq pattern
                   ;; uses a renamed var as its subject. This is safe
                   ;; because only direct subpatterns of an apply
                   ;; pattern get renamed; it should also be safe with
                   ;; any future changes, but this comment is here to
                   ;; remind about this assumption which might break
                   ;; with some admittedly bizarre original
                   ;; core-pattern input
                   (make-seq-pattern (cdr rename)
                                     (seq-pattern-subpat-subject pattern)
                                     (seq-pattern-type pattern)
                                     (seq-pattern-name pattern)
                                     (seq-pattern-state-vars pattern)
                                     (seq-pattern-termination-expr pattern)
                                     (seq-pattern-ref-expr pattern)
                                     (seq-pattern-subpats pattern))))))))

  ;; Renesting ‘and’ patterns has two results: the first is that
  ;; structures like (and (and pat1 pat2) pat3) are rewritten as (and
  ;; pat1 (and pat2 pat3)). The other is that variable and wildcard
  ;; patterns are moved to the end so that high-level patterns like
  ;; (and xs (cons x xs*)) appear as if they were (and (cons x xs*)
  ;; xs). Both of these make it easier for the decision tree generator
  ;; to find and apply specializations.
  (define (pattern-renest-ands pat)
    (ast-cond
        (pattern-renest-ands pat)
      ((and-pattern? pat)
       (let ()
         (define (and-subpats pat)
           (if (and-pattern? pat)
               (append (and-subpats (and-pattern-subpat_1 pat))
                       (and-subpats (and-pattern-subpat_2 pat)))
               (list pat)))
         (define (high-priority? pat)
           (not (or (var-pattern? pat) (wildcard-pattern? pat))))
         (let-values (((high-priority-pats low-priority-pats)
                       (partition high-priority? (and-subpats pat))))
           (reduce-right make-and-pattern
                         #f ; never used
                         (map pattern-renest-ands
                              (append high-priority-pats low-priority-pats))))))))

  (define patacts-renest-ands
    (patacts-pattern-transformation pattern-renest-ands))

  ;; Removing all variables in ‘not’ patterns makes certain subsequent
  ;; operations easier. This pass must precede pattern-de-morgan
  ;; because the latter assumes (not (not pat)) to pat is safe.
  (define (pattern-remove-not-vars pat)
    (ast-cond
        (pattern-remove-not-vars pat)
      ((not-pattern? pat)
       (make-not-pattern
        (pattern-vars->wildcards (not-pattern-subpat pat))))))

  (define (pattern-vars->wildcards pat)
    (ast-cond
        (pattern-vars->wildcards pat)
      ((var-pattern? pat) (make-wildcard-pattern (pattern-subject pat)))))

  (define patacts-remove-not-vars
    (patacts-pattern-transformation pattern-remove-not-vars))

  ;; De Morgan’s laws turn patterns of the form (not (and a b)) into
  ;; (or (not a) (not b)), and patterns of the form (not (or a b))
  ;; into (and (not a) (not b)). This gives the decision tree
  ;; generator a helping hand by reducing the scope of any context
  ;; where the meaning of a pattern is inverted. This pass also makes
  ;; related transformations: (not (apply blah subpat)) into (apply
  ;; blah (not subpat)), and (not (not subpat)) into subpat.
  ;;
  ;; TODO: Investigate the transformation (not (row a ...)) into (or
  ;; (not a) (or ...)). ‘or’ has an order of evaluation restriction
  ;; where ‘row’ doesn’t. Is it worth adding an unordered ‘or’ for
  ;; this case?
  (define (pattern-de-morgan pat)
    (ast-cond
        (pattern-de-morgan pat)
      ((and (not-pattern? pat)
            (and-pattern? (not-pattern-subpat pat)))
       (let* ((subpat_1 (and-pattern-subpat_1 (not-pattern-subpat pat)))
              (subpat_2 (and-pattern-subpat_2 (not-pattern-subpat pat))))
         (pattern-de-morgan
          (make-or-pattern
           (make-not-pattern subpat_1)
           (make-not-pattern subpat_2)))))
      ((and (not-pattern? pat)
            (or-pattern? (not-pattern-subpat pat)))
       (let* ((subpat_1 (or-pattern-subpat_1 (not-pattern-subpat pat)))
              (subpat_2 (or-pattern-subpat_2 (not-pattern-subpat pat))))
         (pattern-de-morgan
          (make-and-pattern
           (make-not-pattern subpat_1)
           (make-not-pattern subpat_2)))))
      ((and (not-pattern? pat)
            (row-pattern? (not-pattern-subpat pat))
            (eqv? (length (row-pattern-subpats (not-pattern-subpat pat))) 1))
       (pattern-de-morgan
        (make-not-pattern
         (car (row-pattern-subpats (not-pattern-subpat pat))))))
      ((and (not-pattern? pat)
            (apply-pattern? (not-pattern-subpat pat)))
       (let ((apply-pat (not-pattern-subpat pat)))
         (pattern-de-morgan
          (make-apply-pattern (pattern-subject apply-pat)
                              (apply-pattern-procedure-id apply-pat)
                              (apply-pattern-procedure-expr apply-pat)
                              (apply-pattern-vars apply-pat)
                              (make-not-pattern
                               (apply-pattern-subpat apply-pat))))))
      ((and (not-pattern? pat)
            (not-pattern? (not-pattern-subpat pat)))
       (pattern-de-morgan (not-pattern-subpat (not-pattern-subpat pat))))))

  (define patacts-de-morgan
    (patacts-pattern-transformation pattern-de-morgan))

  (define (pattern-vars* pattern include-disjointed?)
    (cond ((wildcard-pattern? pattern) '())
          ((var-pattern? pattern) (list (var-pattern-name pattern)))
          ((quote-pattern? pattern) '())
          ((and-pattern? pattern)
           (append (pattern-vars* (and-pattern-subpat_1 pattern)
                                  include-disjointed?)
                   (pattern-vars* (and-pattern-subpat_2 pattern)
                                  include-disjointed?)))
          ((or-pattern? pattern)
           (if include-disjointed?
               (lset-union
                bound-identifier=?
                (pattern-vars* (or-pattern-subpat_1 pattern) #t)
                (pattern-vars* (or-pattern-subpat_2 pattern) #t))
               (lset-intersection
                bound-identifier=?
                (pattern-vars* (or-pattern-subpat_1 pattern) #f)
                (pattern-vars* (or-pattern-subpat_2 pattern) #f))))
          ((row-pattern? pattern)
           (append-map (lambda (subpat)
                         (pattern-vars* subpat include-disjointed?))
                       (row-pattern-subpats pattern)))
          ((not-pattern? pattern) '())
          ((?-pattern? pattern) '())
          ((apply-pattern? pattern)
           (pattern-vars* (apply-pattern-subpat pattern) include-disjointed?))
          ((seq-pattern? pattern)
           (append-map (lambda (seq-subpat)
                         (seq-pattern-vars* seq-subpat include-disjointed?))
                       (seq-pattern-subpats pattern)))
          (else (assertion-violation 'pattern-vars* "not a pattern" pattern))))

  (define (seq-pattern-vars* seq-subpat include-disjointed?)
    (pattern-vars* (cond ((seq-pattern-one? seq-subpat)
                          (seq-pattern-one-subpat seq-subpat))
                         ((seq-pattern-many? seq-subpat)
                          (seq-pattern-many-subpat seq-subpat))
                         (else (assertion-violation 'seq-pattern-vars
                                                    "not a sequence subpattern"
                                                    seq-subpat)))
                   include-disjointed?))

  (define (pattern-vars pattern) (pattern-vars* pattern #f))
  (define (pattern-disjointed-vars pattern)
    (lset-difference bound-identifier=?
                     (pattern-vars* pattern #t)
                     (pattern-vars* pattern #f)))
  (define (seq-pattern-vars seq-subpat) (seq-pattern-vars* seq-subpat #f))

  (define (raise-if-non-linear pattern)
    (raise-if-non-linear* pattern '()))
  (define (raise-if-non-linear* pattern already-used)
    (define (subpattern-vars subpat)
      (if (not-pattern? subpat)
          (subpattern-vars (not-pattern-subpat subpat))
          (pattern-vars subpat)))
    (ast-cond ((lambda (subpat)
                 (raise-if-non-linear* subpat already-used))
               pattern)
      ((var-pattern? pattern)
       (when (member (var-pattern-name pattern)
                     already-used
                     bound-identifier=?)
         (syntax-violation 'match
                           "non-linear pattern with variable"
                           (var-pattern-name pattern))))
      ((and-pattern? pattern)
       (raise-if-non-linear* (and-pattern-subpat_1 pattern)
                             already-used)
       (raise-if-non-linear* (and-pattern-subpat_2 pattern)
                             (append (subpattern-vars
                                      (and-pattern-subpat_1 pattern))
                                     already-used)))
      ((row-pattern? pattern)
       (fold-left (lambda (already-used subpat)
                    (raise-if-non-linear* subpat already-used)
                    (append (subpattern-vars subpat) already-used))
                  already-used
                  (row-pattern-subpats pattern)))
      ((seq-pattern? pattern)
       (fold-left (lambda (already-used seq-subpat)
                    (let ((subpat
                           (if (seq-pattern-one? seq-subpat)
                               (seq-pattern-one-subpat seq-subpat)
                               (seq-pattern-many-subpat seq-subpat))))
                      (raise-if-non-linear* subpat already-used)
                      (append (subpattern-vars subpat) already-used)))
                  already-used
                  (seq-pattern-subpats pattern))))))
