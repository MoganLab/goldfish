(library (extensible-match decision-tree)
  (export patacts->decision-tree
          pattern->dt-node
          prepare-patacts
          *dt-hash*
          clear-decision-tree-cache!
          pattern-can-specialize?
          pattern-should-specialize?
          specializer-score
          specialize-patacts
          default-patacts
          find-best-specializer
          complementary-patterns?
          remove-decision-tree-renames

          dt-node?
          dt-node-hash
          dt-node-success-branch
          dt-node-failure-branch
          dt-test?
          dt-test-proc
          dt-test-var
          dt-apply?
          dt-apply-proc
          dt-apply-var
          dt-apply-vars
          dt-equal?
          dt-equal-val
          dt-equal-var
          dt-rename?
          dt-rename-internal
          dt-rename-external
          dt-seq?
          dt-seq-var
          dt-seq-pattern
          dt-hash
          dt=?

          make-action
          action?
          action-procedure
          action-args
          failure-action

          decision-tree->scheme
          decision-tree->dot)
  (import (except (rnrs (6)) assoc member partition)
          (only (srfi :1 lists)
                append-map assoc
                break
                any every
                list=
                car+cdr
                first)
          (extensible-match ast)
          (extensible-match util))

  ;; A decision tree is the third layer of intermediate representation
  ;; beyond the AST. Compared to core-patterns and the AST, they have
  ;; the advantage that common subpatterns between patterns within the
  ;; same instance of ‘match’ are identified and reduced so they
  ;; ideally are only evaluated once.
  ;;
  ;; The transformation from a decision tree to Scheme code is quite
  ;; trivial. The transformation from a set of patterns to a decision
  ;; tree is much trickier.
  ;;
  ;; This is some of the hardest code I have ever had to write.

  (define-syntax dt-hash-cons
    (syntax-rules ()
      ((_ type-test hash-proc same?-proc field ...)
       (lambda (p)
         (lambda (s f field ...)
           (let* ((h (hash-proc s f field ...)))
             (let* ((bucket (hashtable-ref *dt-hash* h #f)))
               (if bucket
                   (let ((r_existing
                          (find
                           (lambda (x)
                             (and (type-test x)
                                  (same?-proc x s f field ...)))
                           bucket)))
                     (if r_existing
                         r_existing
                         (let ((r_new ((p h s f) field ...)))
                           (hashtable-set! *dt-hash* h
                                           (cons r_new bucket))
                           r_new)))
                   (let ((r ((p h s f) field ...)))
                     (hashtable-set! *dt-hash* h (list r))
                     r)))))))))

  (define-record-type dt-node
    (fields hash success-branch failure-branch))
  (define-record-type dt-test
    (fields proc var)
    (parent dt-node)
    (protocol (dt-hash-cons dt-test? dt-test-hash dt-test=/for-hash?
                            proc var)))
  (define-record-type dt-apply
    (fields proc var vars)
    (parent dt-node)
    (protocol (dt-hash-cons dt-apply? dt-apply-hash dt-apply=/for-hash?
                            proc var vars)))
  (define-record-type dt-equal
    (fields val var)
    (parent dt-node)
    (protocol (dt-hash-cons dt-equal? dt-equal-hash dt-equal=/for-hash?
                            val var)))
  (define-record-type dt-rename
    (fields internal external)
    (parent dt-node)
    (protocol (dt-hash-cons dt-rename? dt-rename-hash dt-rename=/for-hash?
                            val var)))
  (define-record-type dt-seq
    (fields var pattern)
    (parent dt-node)
    (protocol (dt-hash-cons dt-seq? dt-seq-hash dt-seq=/for-hash?
                            var pattern)))

  (define (dt-test-hash s f proc var)
    (hash-combine (dt-hash s)
                  (dt-hash f)
                  (datum-hash (syntax->datum proc))
                  (bound-identifier-hash var)))
  (define (dt-test=/for-hash? dt s f proc var)
    (and (dt=? s (dt-node-success-branch dt))
         (dt=? f (dt-node-failure-branch dt))
         (bound-identifier=? var (dt-test-var dt))
         (dt-expr=? proc (dt-test-proc dt))))

  (define (dt-apply-hash s f proc var vars)
    (hash-combine (dt-hash s)
                  #;(datum-hash (syntax->datum proc))
                  (bound-identifier-hash var)
                  (fold-left bitwise-xor
                             0
                             (map bound-identifier-hash vars))))
  (define (dt-apply=/for-hash? dt s f proc var vars)
    (and (dt=? s (dt-node-success-branch dt))
         (bound-identifier=? var (dt-apply-var dt))
         (list= bound-identifier=?
                vars
                (dt-apply-vars dt))
         (dt-expr=? proc (dt-apply-proc dt))))

  (define (dt-equal-hash s f val var)
    (hash-combine (dt-hash s)
                  (dt-hash f)
                  (datum-hash (syntax->datum val))
                  (bound-identifier-hash var)))
  (define (dt-equal=/for-hash? dt s f val var)
    (and (dt=? s (dt-node-success-branch dt))
         (dt=? f (dt-node-failure-branch dt))
         (bound-identifier=? var (dt-equal-var dt))
         (equal? val (syntax->datum (dt-equal-val dt)))))

  (define (dt-rename-hash s f internal external)
    (hash-combine (dt-hash s)
                  (bound-identifier-hash internal)
                  (bound-identifier-hash external)))
  (define (dt-rename=/for-hash? dt s f internal external)
    (and (dt=? s (dt-node-success-branch dt))
         (bound-identifier=? internal (dt-rename-internal dt))
         (bound-identifier=? external (dt-rename-external dt))))

  (define (dt-seq-hash s f var pattern)
    (hash-combine (dt-hash s)
                  (dt-hash f)
                  (bound-identifier-hash var)
                  (ast-hash pattern)))
  (define (dt-seq=/for-hash? dt s f var pattern)
    (and (dt=? s (dt-node-success-branch dt))
         (dt=? f (dt-node-failure-branch dt))
         (bound-identifier=? var (dt-seq-var dt))
         (eqv? pattern (dt-seq-pattern dt))))

  (define (dt-hash dt)
    (cond ((dt-node? dt) (dt-node-hash dt))
          (else 0)))

  (define (dt-expr=? expr_1 expr_2)
    (or (and (identifier? expr_1)
             (identifier? expr_2)
             (free-identifier=? expr_1 expr_2))
        (eqv? expr_2 expr_1)))

  (define (dt=? dt_1 dt_2)
    (assert (and (or (dt-node? dt_1)
                     (action? dt_1))
                 (or (dt-node? dt_2)
                     (action? dt_2))))
    (or (eq? dt_1 dt_2)
        (and (action? dt_1)
             (action? dt_2)
             (ast=? dt_1 dt_2))
        (and
         (dt-node? dt_1)
         (dt-node? dt_2)
         (fx=? (dt-node-hash dt_1) (dt-node-hash dt_2))
         (or
          (and (dt-test? dt_1)
               (dt-test? dt_2)
               (dt-test=/for-hash? dt_1
                                   (dt-node-success-branch dt_2)
                                   (dt-node-failure-branch dt_2)
                                   (dt-test-proc dt_2)
                                   (dt-test-var dt_2)))
          (and (dt-apply? dt_1)
               (dt-apply? dt_2)
               (dt-apply=/for-hash? dt_1
                                    (dt-node-success-branch dt_2)
                                    #f
                                    (dt-apply-proc dt_2)
                                    (dt-apply-var dt_2)
                                    (dt-apply-vars dt_2)))
          (and (dt-equal? dt_1)
               (dt-equal? dt_2)
               (dt-equal=/for-hash? dt_1
                                    (dt-node-success-branch dt_2)
                                    (dt-node-failure-branch dt_2)
                                    (dt-equal-val dt_2)
                                    (dt-equal-var dt_2)))
          (and (dt-rename? dt_1)
               (dt-rename? dt_2)
               (dt-rename=/for-hash? dt_1
                                     (dt-node-success-branch dt_2)
                                     #f
                                     (dt-rename-internal dt_2)
                                     (dt-rename-external dt_2)))
          (and (dt-seq? dt_1)
               (dt-seq? dt_2)
               (dt-seq=/for-hash? dt_1
                                  (dt-node-success-branch dt_2)
                                  (dt-node-failure-branch dt_2)
                                  (dt-seq-var dt_2)
                                  (dt-seq-pattern dt_2)))))))

  (define *dt-hash* (make-hashtable values fx=?))
  (define *patacts->dt-memo*
    (make-hashtable (lambda (patacts)
                      (fold-right (lambda (patact hash)
                                    (hash-combine (ast-hash patact) hash))
                                  0
                                  patacts))
                    (lambda (a b)
                      (list= ast=? a b))))

  ;; patacts->decision-tree handles cases where optimizations can be
  ;; made by comparing adjacent patterns with one another to find
  ;; similarities. Its results are hash-consed, so that the tree will
  ;; have reasonably optimal sharing and not needlessly duplicate code;
  ;; its result is also memoized, saving it from having to even undergo
  ;; the expensive analysis process when two steps create identical
  ;; reduced cases.
  (define (patacts->decision-tree patacts)
    (let ((dt (patacts->decision-tree/memo patacts)))
      (clear-decision-tree-cache!)
      dt))
  (define (patacts->decision-tree/memo patacts)
    (cond ((hashtable-ref *patacts->dt-memo*
                          patacts
                          #f)
           => values)
          (else
           (let ((dt (patacts->decision-tree/step patacts)))
             (hashtable-set! *patacts->dt-memo* patacts dt)
             dt))))
  (define (patacts->decision-tree/step patacts)
    (let ((patacts (prepare-patacts patacts)))
      ;; The guts of the optimized derivation of decision trees is
      ;; based on the recursive algorithm presented by Luc Maranget
      ;; in:
      ;;
      ;; ‘Compiling Pattern Matching to Good Decision Trees’
      ;; <http://moscova.inria.fr/~maranget/papers/ml05e-maranget.pdf>
      ;; (ML’08 proceedings pp. 35–46; DOI 10.1145/1411304.1411311)
      ;;
      ;; Maranget’s presentation assumes an ML-like source language;
      ;; we have to make some adaptations to Scheme, where typing is
      ;; done by predicate procedures. This is handled earlier by the
      ;; combination of the core:and and core:? core-patterns, of
      ;; course, but then in order to be able to apply Maranget‘s
      ;; algorithm successfully given our set of core-patterns, we
      ;; have to group adjacent core:and patterns whose left-hand
      ;; subpatterns are the same (test for the same core:?
      ;; procedure), then recurse into optimizing the right-hand sides
      ;; as a collection of ‘rows’.
      ;;
      ;; ‘Column swapping’ and the notion that a set of patterns has
      ;; to form a coherent grid are considered here to be
      ;; implementation details of Maranget’s original presentation
      ;; and are not replicated. Instead, we do a rather messy search
      ;; through a set of ‘rows’. The only constraint is that the node
      ;; we generate has to correspond to a pattern that was
      ;; originally in the top row, otherwise we break the guarantee
      ;; that patterns are tested in order from top to bottom.
      (cond ((null? patacts) failure-action)
            ((not (any pattern-should-specialize?
                       (row-pattern-subpats (patact-pattern (first patacts)))))
             (pattern->dt-node
              (patact-pattern (first patacts))
              (patact-action (first patacts))
              ;; Variation from Maranget’s original: a pattern can
              ;; still fail at this point because it might contain
              ;; subpatterns we can’t optimize for with this algorithm
              ;; (seq patterns, not patterns)
              (patacts->decision-tree/memo (cdr patacts))))
            (else
             (let* ((specializer (find-best-specializer patacts))
                    (specialized (specialize-patacts specializer patacts))
                    (defaulted (default-patacts specializer (cdr patacts))))
               (pattern->dt-node
                specializer
                (patacts->decision-tree/memo specialized)
                (patacts->decision-tree/memo defaulted)))))))

  (define (clear-decision-tree-cache!)
    (hashtable-clear! *dt-hash*)
    (hashtable-clear! *patacts->dt-memo*))

  ;; pattern->dt-node handles the ‘naïve’ case where no (further)
  ;; comparison with adjacent patterns is done; it is also used in the
  ;; generation of test procedures for seq pattern matching
  (define (pattern->dt-node pattern success-branch failure-branch)
    (cond ((wildcard-pattern? pattern) success-branch)
          ((var-pattern? pattern)
           (make-dt-rename success-branch #f ; failure can never happen
                           (pattern-subject pattern)
                           (var-pattern-name pattern)))
          ((quote-pattern? pattern)
           (make-dt-equal success-branch failure-branch
                          (quote-pattern-datum pattern)
                          (pattern-subject pattern)))
          ((and-pattern? pattern)
           (pattern->dt-node
            (and-pattern-subpat_1 pattern)
            (pattern->dt-node (and-pattern-subpat_2 pattern)
                              success-branch
                              failure-branch)
            failure-branch))
          ((or-pattern? pattern)
           (pattern->dt-node
            (or-pattern-subpat_1 pattern)
            success-branch
            (pattern->dt-node (or-pattern-subpat_2 pattern)
                              success-branch
                              failure-branch)))
          ((row-pattern? pattern)
           (fold-right
            (lambda (subpat success-branch)
              (pattern->dt-node subpat
                                success-branch
                                failure-branch))
            success-branch
            (row-pattern-subpats pattern)))
          ((not-pattern? pattern)
           (pattern->dt-node (not-pattern-subpat pattern)
                             failure-branch
                             success-branch))
          ((?-pattern? pattern)
           (make-dt-test success-branch failure-branch
                         (?-pattern-predicate-expr pattern)
                         (pattern-subject pattern)))
          ((apply-pattern? pattern)
           (make-dt-apply (pattern->dt-node (apply-pattern-subpat pattern)
                                            success-branch
                                            failure-branch)
                          #f ; failure can never happen
                          (apply-pattern-procedure-expr pattern)
                          (pattern-subject pattern)
                          (apply-pattern-vars pattern)))
          ((seq-pattern? pattern)
           (make-dt-seq success-branch failure-branch
                        (pattern-subject pattern)
                        pattern))
          (else (assertion-violation 'pattern->dt-node
                                     "not a pattern"
                                     pattern))))

  ;;; Source-level transformations on patacts

  ;; We do three initial preparations: turn ‘or’ patterns into a
  ;; series of separate consecutive patterns which point to the same
  ;; action; flatted nested row pattern; and turn patterns which
  ;; aren’t in a row pattern into single-item ‘rows’. These are all in
  ;; service of better identifying commonalities between
  ;; (sub)patterns.

  ;; Iterate the preparation to a fixed point where we know there is
  ;; nothing more to do.
  (define (prepare-patacts patacts)
    (let loop ((old patacts))
      (let ((new (prepare-patacts* old)))
        (if (list= ast=? old new)
            new
            (loop new)))))

  (define (prepare-patacts* patacts)
    (define (and-pattern/or-pattern? pat)
      (and (and-pattern? pat)
           (or-pattern? (and-pattern-subpat_1 pat))))
    (append-map
     (lambda (patact)
       (let ((pattern (patact-pattern patact)))
         (cond ((not (row-pattern? pattern))
                (list
                 (make-patact (make-row-pattern (list pattern))
                              (patact-action patact))))
               ((any or-pattern? (row-pattern-subpats pattern))
                (let*-values (((pre-pats or-pat+post-pats)
                               (break or-pattern?
                                      (row-pattern-subpats pattern)))
                              ((or-pat post-pats)
                               (car+cdr or-pat+post-pats)))
                  (list
                   (make-patact (make-row-pattern
                                 (append pre-pats
                                         (list (or-pattern-subpat_1 or-pat))
                                         post-pats))
                                (patact-action patact))
                   (make-patact (make-row-pattern
                                 (append pre-pats
                                         (list (or-pattern-subpat_2 or-pat))
                                         post-pats))
                                (patact-action patact)))))
               ((any and-pattern/or-pattern? (row-pattern-subpats pattern))
                (let*-values (((pre-pats andor-pat+post-pats)
                               (break and-pattern/or-pattern?
                                      (row-pattern-subpats pattern)))
                              ((andor-pat post-pats)
                               (car+cdr andor-pat+post-pats))
                              ((or-subpat_1)
                               (or-pattern-subpat_1
                                (and-pattern-subpat_1 andor-pat)))
                              ((or-subpat_2)
                               (or-pattern-subpat_2
                                (and-pattern-subpat_1 andor-pat)))
                              ((and-subpat_2)
                               (and-pattern-subpat_2 andor-pat)))
                  (list
                   (make-patact (make-row-pattern
                                 (append pre-pats
                                         (list
                                          (make-and-pattern
                                           or-subpat_1
                                           and-subpat_2))
                                         post-pats))
                                (patact-action patact))
                   (make-patact (make-row-pattern
                                 (append pre-pats
                                         (list
                                          (make-and-pattern or-subpat_2
                                                            and-subpat_2))
                                         post-pats))
                                (patact-action patact)))))
               ((any row-pattern? (row-pattern-subpats pattern))
                (let*-values (((pre-pats row-pat+post-pats)
                               (break row-pattern?
                                      (row-pattern-subpats pattern)))
                              ((row-pat post-pats)
                               (car+cdr row-pat+post-pats)))
                  (list (make-patact
                         (make-row-pattern
                          (append pre-pats
                                  (row-pattern-subpats row-pat)
                                  post-pats))
                         (patact-action patact)))))
               (else (list patact)))))
     patacts))

  ;;;
  (define (specialize-patacts specializer patacts)
    (define (specialize-subpat subpat)
      (if (and (pattern-can-specialize? subpat)
               (specializer=? (pattern->specializer subpat)
                              specializer))
          (apply-specializer subpat)
          (list subpat)))
    (append-map
     (lambda (patact)
       (cond ((has-specializer? (patact-pattern patact)
                                specializer
                                pattern-can-specialize?)
              (list (make-patact
                     (make-row-pattern
                      (append-map specialize-subpat
                                  (row-pattern-subpats (patact-pattern patact))))
                     (patact-action patact))))
             ((has-complementary-pattern? (patact-pattern patact)
                                          specializer)
              '())
             (else (list patact))))
     patacts))

  (define (default-patacts specializer patacts)
    (remp
     (lambda (patact)
       (has-specializer? (patact-pattern patact)
                         specializer
                         pattern-can-specialize?))
     patacts))

  ;;; Helper functions for our version of Maranget’s algorithm

  (define (find-best-specializer patacts)
    (let loop ((more-subpats (row-pattern-subpats
                              (patact-pattern
                               (car patacts))))
               (most-effective-specializer #f)
               (most-effective-specializer-score (cons -inf.0 -inf.0)))
      (cond ((null? more-subpats)
             most-effective-specializer)
            ((pattern-should-specialize? (car more-subpats))
             (let* ((specializer (pattern->specializer (car more-subpats)))
                    (score (specializer-score specializer patacts)))
               (if (score< most-effective-specializer-score score)
                   (loop (cdr more-subpats)
                         specializer
                         score)
                   (loop (cdr more-subpats)
                         most-effective-specializer
                         most-effective-specializer-score))))
            (else (loop (cdr more-subpats)
                        most-effective-specializer
                        most-effective-specializer-score)))))

  (define (score< a b)
    (or (< (car a) (car b))
        (< (cdr a) (cdr b))))

  (define (specializer-score specializer patacts)
    (cons (specializer-needed-rows specializer patacts)
          (- (+ (length (specialize-patacts specializer patacts))
                (length (default-patacts specializer (cdr patacts)))))))

  (define (specializer-needed-rows specializer patacts)
    (cond ((null? patacts) 0)
          ((has-specializer? (patact-pattern (car patacts))
                             specializer
                             pattern-should-specialize?)
           (+ 1 (specializer-needed-rows specializer (cdr patacts))))
          (else (specializer-needed-rows specializer (cdr patacts)))))

  (define (has-specializer? row specializer can/should?)
    (any (lambda (subpat)
           (and (can/should? subpat)
                (specializer=? (pattern->specializer subpat)
                               specializer)))
         (row-pattern-subpats row)))

  (define (pattern->specializer pat)
    (cond ((quote-pattern? pat) pat)
          ((and-pattern? pat)
           (pattern->specializer (and-pattern-subpat_1 pat)))
          ((?-pattern? pat) pat)
          ((apply-pattern? pat)
           (make-apply-pattern
            (pattern-subject pat)
            (apply-pattern-procedure-id pat)
            (apply-pattern-procedure-expr pat)
            (apply-pattern-vars pat)
            (make-wildcard-pattern
             (pattern-subject pat))))
          ((and (not-pattern? pat)
                (or (quote-pattern? (not-pattern-subpat pat))
                    (?-pattern? (not-pattern-subpat pat))))
           pat)
          (else (assertion-violation 'pattern->specializer
                                     "pattern type not specializable"
                                     pat))))

  (define (pattern-should-specialize? pat)
    (and (not (pattern-irrefutable? pat))
         (pattern-can-specialize? pat)))
  (define (pattern-can-specialize? pat)
    (or (quote-pattern? pat)
        (and (and-pattern? pat)
             (pattern-can-specialize? (and-pattern-subpat_1 pat)))
        (?-pattern? pat)
        (apply-pattern? pat)
        (and (not-pattern? pat)
             (or (quote-pattern? (not-pattern-subpat pat))
                 (?-pattern? (not-pattern-subpat pat))))))

  (define (pattern-irrefutable? pat)
    (or (wildcard-pattern? pat)
        (var-pattern? pat)
        (and (and-pattern? pat)
             (pattern-irrefutable? (and-pattern-subpat_1 pat))
             (pattern-irrefutable? (and-pattern-subpat_2 pat)))
        (and (or-pattern? pat)
             (or (pattern-irrefutable? (or-pattern-subpat_1 pat))
                 (pattern-irrefutable? (or-pattern-subpat_2 pat))))
        (and (row-pattern? pat)
             (every pattern-irrefutable? (row-pattern-subpats pat)))
        (and (apply-pattern? pat)
             (pattern-irrefutable? (apply-pattern-subpat pat)))))

  (define (specializer=? pat_1 pat_2)
    (or (and (not-pattern? pat_1)
             (not-pattern? pat_2)
             (specializer=? (not-pattern-subpat pat_1)
                            (not-pattern-subpat pat_2)))
        (and (pattern? pat_1)
             (pattern? pat_2)
             (bound-identifier=? (pattern-subject pat_1)
                                 (pattern-subject pat_2))
             (or (and (apply-pattern? pat_1)
                      (apply-pattern? pat_2)
                      (fx=? (apply-pattern-procedure-id pat_1)
                            (apply-pattern-procedure-id pat_2))
                      (list= bound-identifier=?
                             (apply-pattern-vars pat_1)
                             (apply-pattern-vars pat_2)))
                 (and (quote-pattern? pat_1)
                      (quote-pattern? pat_2)
                      (equal? (quote-pattern-datum pat_1)
                              (quote-pattern-datum pat_2)))
                 (and (?-pattern? pat_1)
                      (?-pattern? pat_2)
                      (fx=? (?-pattern-predicate-id pat_1)
                            (?-pattern-predicate-id pat_2)))))))

  (define (apply-specializer pat)
    (cond ((quote-pattern? pat) '())
          ((and-pattern? pat)
           (let ((specialized
                  (apply-specializer (and-pattern-subpat_1 pat))))
             (if (null? specialized)
                 (list (and-pattern-subpat_2 pat))
                 (list (make-and-pattern
                        (if (null? (cdr specialized))
                            (car specialized)
                            (make-row-pattern specialized))
                        (and-pattern-subpat_2 pat))))))
          ((?-pattern? pat) '())
          ((apply-pattern? pat) (list (apply-pattern-subpat pat)))
          ((not-pattern? pat) '())))

  (define (has-complementary-pattern? row specializer)
    (any (lambda (subpat)
           (complementary-patterns? subpat specializer))
         (row-pattern-subpats row)))

  (define datum-predicates
    ;; Meta-level–breaking hack to detect when simple ? patterns and
    ;; quote patterns are complementary.
    ;;
    ;; Entries in this list must take a single argument and return a
    ;; boolean. The argument must be able to be any Scheme datum
    ;; without an error; hence integer? is allowed but exact? is not;
    ;; procedure? and hashtable? are not here because those types are
    ;; not datums.
    ;;
    ;; This might be replaced in a future version by use of identifier
    ;; properties to make this list extensible.
    (list
     (cons #'boolean? boolean?)
     (cons #'bytevector? bytevector?)
     (cons #'char? char?)
     (cons #'complex? complex?)
     (cons #'fixnum? fixnum?)
     (cons #'flonum? flonum?)
     (cons #'integer? integer?)
     (cons #'integer-valued? integer-valued?)
     (cons #'list? list?)
     (cons #'null? null?)
     (cons #'number? number?)
     (cons #'pair? pair?)
     (cons #'rational? rational?)
     (cons #'rational-valued? rational-valued?)
     (cons #'real? real?)
     (cons #'real-valued? real-valued?)
     (cons #'string? string?)
     (cons #'symbol? symbol?)
     (cons #'vector? vector?)
     ))

  (define (complementary-patterns? pat_1 pat_2)
    (cond ((and-pattern? pat_1)
           (or (complementary-patterns? (and-pattern-subpat_1 pat_1)
                                        pat_2)
               (complementary-patterns? (and-pattern-subpat_2 pat_1)
                                        pat_2)))
          ((and-pattern? pat_2)
           (or (complementary-patterns? (and-pattern-subpat_1 pat_2)
                                        pat_1)
               (complementary-patterns? (and-pattern-subpat_2 pat_2)
                                        pat_1)))
          ((row-pattern? pat_1)
           (any (lambda (subpat)
                  (complementary-patterns? subpat pat_2))
                (row-pattern-subpats pat_1)))
          ((row-pattern? pat_2)
           (any (lambda (subpat)
                  (complementary-patterns? pat_1 subpat))
                (row-pattern-subpats pat_2)))
          ;; Protect the next two clauses from diverging
          ((and (apply-pattern? pat_1)
                (?-pattern? pat_2))
           (complementary-patterns? (apply-pattern-subpat pat_1)
                                    pat_2))
          ((and (not (apply-pattern? pat_1))
                (apply-pattern? pat_2))
           (complementary-patterns? pat_2 pat_1))
          ((and (not (?-pattern? pat_1))
                (?-pattern? pat_2))
           (complementary-patterns? pat_2 pat_1))
          (else
           (or (and (apply-pattern? pat_1)
                    (complementary-patterns? (apply-pattern-subpat pat_1)
                                             pat_2))
               (and (pattern? pat_1)
                    (pattern? pat_2)
                    (bound-identifier=? (pattern-subject pat_1)
                                        (pattern-subject pat_2))
                    (or (and (quote-pattern? pat_1)
                             (quote-pattern? pat_2)
                             (not (equal? (quote-pattern-datum pat_1)
                                          (quote-pattern-datum pat_2))))
                        (and (?-pattern? pat_1)
                             (quote-pattern? pat_2)
                             (let ((datum-predicate
                                    (assoc (?-pattern-predicate-expr pat_1)
                                           datum-predicates
                                           dt-expr=?)))
                               (and datum-predicate
                                    (not ((cdr datum-predicate)
                                          (quote-pattern-datum pat_2))))))))))))

  ;;; Remove rename nodes

  (define (remove-decision-tree-renames dt)
    (cond ((dt-test? dt)
           (make-dt-test
            (remove-decision-tree-renames (dt-node-success-branch dt))
            (remove-decision-tree-renames (dt-node-failure-branch dt))
            (dt-test-proc dt)
            (dt-test-var dt)))
          ((dt-apply? dt)
           (make-dt-apply
            (remove-decision-tree-renames (dt-node-success-branch dt))
            #f
            (dt-apply-proc dt)
            (dt-apply-var dt)
            (dt-apply-vars dt)))
          ((dt-equal? dt)
           (make-dt-equal
            (remove-decision-tree-renames (dt-node-success-branch dt))
            (remove-decision-tree-renames (dt-node-failure-branch dt))
            (dt-equal-val dt)
            (dt-equal-var dt)))
          ((dt-rename? dt)
           (remove-decision-tree-renames
            (remove-decision-tree-rename (dt-node-success-branch dt)
                                         (dt-rename-external dt)
                                         (dt-rename-internal dt))))
          ((dt-seq? dt)
           (make-dt-seq
            (remove-decision-tree-renames (dt-node-success-branch dt))
            (remove-decision-tree-renames (dt-node-failure-branch dt))
            (dt-seq-var dt)
            (dt-seq-pattern dt)))
          ((action? dt) dt)
          (else (assertion-violation 'remove-decision-tree-renames
                                     "not a dt node"
                                     dt))))

  (define (remove-decision-tree-rename dt old new)
    (cond ((dt-test? dt)
           (make-dt-test
            (remove-decision-tree-rename (dt-node-success-branch dt)
                                         old new)
            (remove-decision-tree-rename (dt-node-failure-branch dt)
                                         old new)
            (dt-test-proc dt)
            (if (bound-identifier=? (dt-test-var dt) old)
                new
                (dt-test-var dt))))
          ((dt-apply? dt)
           (make-dt-apply
            (remove-decision-tree-rename (dt-node-success-branch dt)
                                         old new)
            #f
            (dt-apply-proc dt)
            (if (bound-identifier=? (dt-apply-var dt) old)
                new
                (dt-apply-var dt))
            (dt-apply-vars dt)))
          ((dt-equal? dt)
           (make-dt-equal
            (remove-decision-tree-rename (dt-node-success-branch dt)
                                         old new)
            (remove-decision-tree-rename (dt-node-failure-branch dt)
                                         old new)
            (dt-equal-val dt)
            (if (bound-identifier=? (dt-equal-var dt) old)
                new
                (dt-equal-var dt))))
          ((dt-rename? dt)
           (if (bound-identifier=? (dt-rename-internal dt) old)
               (remove-decision-tree-rename
                (remove-decision-tree-rename dt (dt-rename-internal dt) new)
                old new)
               (make-dt-rename
                (remove-decision-tree-rename (dt-node-success-branch dt)
                                             old new)
                #f
                (dt-rename-internal dt)
                (dt-rename-external dt))))
          ((dt-seq? dt)
           (make-dt-seq
            (remove-decision-tree-rename (dt-node-success-branch dt)
                                         old new)
            (remove-decision-tree-rename (dt-node-failure-branch dt)
                                         old new)
            (if (bound-identifier=? (dt-seq-var dt) old)
                new
                (dt-seq-var dt))
            (dt-seq-pattern dt)))
          ((eq? dt failure-action) failure-action)
          ((action? dt)
           (make-action (action-procedure dt)
                        (map (lambda (arg)
                               (if (bound-identifier=? arg old)
                                   new
                                   arg))
                             (action-args dt))))
          (else (assertion-violation 'remove-decision-tree-rename
                                     "not a dt node"
                                     dt))))

  ;;; Aids to interpreting a decision tree

  (define (decision-tree->scheme dt)
    (cond ((dt-test? dt)
           `(if (,(dt-test-proc dt) ,(dt-test-var dt))
                ,(decision-tree->scheme (dt-node-success-branch dt))
                ,(decision-tree->scheme (dt-node-failure-branch dt))))
          ((dt-apply? dt)
           (if (= (length (dt-apply-vars dt)) 1)
               `(let ((,(car (dt-apply-vars dt))
                       (,(dt-apply-proc dt) ,(dt-apply-var dt))))
                  ,(decision-tree->scheme (dt-node-success-branch dt)))
               `(let-values ((,(dt-apply-vars dt)
                              (,(dt-apply-proc dt) ,(dt-apply-var dt))))
                  ,(decision-tree->scheme (dt-node-success-branch dt)))))
          ((dt-equal? dt)
           `(if (equal? ,(dt-equal-var dt) ',(dt-equal-val dt))
                ,(decision-tree->scheme (dt-node-success-branch dt))
                ,(decision-tree->scheme (dt-node-failure-branch dt))))
          ((dt-rename? dt)
           `(let ((,(dt-rename-external dt) ,(dt-rename-internal dt)))
              ,(decision-tree->scheme (dt-node-success-branch dt))))
          ((dt-seq? dt)
           'not-implemented-yet)
          (else (syntax->datum dt))))

  (define (decision-tree->dot dt)
    (define done-set (make-eq-hashtable))
    (define id-table (make-eq-hashtable))
    (define max-id 0)
    (define (done? node)
      (hashtable-contains? done-set node))
    (define (done! node)
      (hashtable-set! done-set node #t))
    (define (write-id node)
      (let ((id (if (hashtable-contains? id-table node)
                    (hashtable-ref id-table node #f)
                    (begin
                      (set! max-id (+ max-id 1))
                      (hashtable-set! id-table node max-id)
                      max-id))))
        (display "n")
        (display (number->string id 16))))
    (display "digraph DT {")
    (newline)
    (let recur ((node dt))
      (unless (done? node)
        (done! node)
        (cond ((dt-test? node)
               (write-id node)
               (display "[label=\"(")
               (write (syntax->datum (dt-test-proc node)))
               (display #\space)
               (write (syntax->datum (dt-test-var node)))
               (display ")\", shape=diamond];")
               (newline)
               (write-id node)
               (display " -> ")
               (write-id (dt-node-success-branch node))
               (display " [label=\"T\"];")
               (newline)
               (write-id node)
               (display " -> ")
               (write-id (dt-node-failure-branch node))
               (display " [label=\"F\"];")
               (newline)
               (recur (dt-node-success-branch node))
               (recur (dt-node-failure-branch node)))
              ((dt-apply? node)
               (write-id node)
               (display "[label=\"(receive ")
               (write (syntax->datum (dt-apply-vars node)))
               (display " (")
               (write (syntax->datum (dt-apply-proc node)))
               (display #\space)
               (write (syntax->datum (dt-apply-var node)))
               (display ") ...)\", shape=rectangle];")
               (newline)
               (write-id node)
               (display " -> ")
               (write-id (dt-node-success-branch node))
               (display ";")
               (newline)
               (recur (dt-node-success-branch node)))
              ((dt-equal? node)
               (write-id node)
               (display "[label=\"(equal? ")
               (write (syntax->datum (dt-equal-var node)))
               (display " '")
               (write (syntax->datum (dt-equal-val node)))
               (display ")\", shape=diamond];")
               (newline)
               (write-id node)
               (display " -> ")
               (write-id (dt-node-success-branch node))
               (display " [label=\"T\"];")
               (newline)
               (write-id node)
               (display " -> ")
               (write-id (dt-node-failure-branch node))
               (display " [label=\"F\"];")
               (newline)
               (recur (dt-node-success-branch node))
               (recur (dt-node-failure-branch node)))
              ((dt-rename? node)
               (write-id node)
               (display "[label=\"(let ((")
               (write (syntax->datum (dt-rename-external node)))
               (display #\space)
               (write (syntax->datum (dt-rename-internal node)))
               (display ")) ...)\", shape=rectangle];")
               (newline)
               (write-id node)
               (display " -> ")
               (write-id (dt-node-success-branch node))
               (display ";")
               (newline)
               (recur (dt-node-success-branch node)))
              ((dt-seq? node)
               (write-id node)
               (display "[label=\"... sequence pattern ...\"];")
               (newline)
               (write-id node)
               (display " -> ")
               (write-id (dt-node-success-branch node))
               (display " [label=\"T\"];")
               (newline)
               (write-id node)
               (display " -> ")
               (write-id (dt-node-failure-branch node))
               (display " [label=\"F\"];")
               (newline)
               (recur (dt-node-success-branch node))
               (recur (dt-node-failure-branch node)))
              ((action? node)
               (write-id node)
               (display "[label=\"")
               (display "(")
               (write (syntax->datum (action-procedure node)))
               (for-each (lambda (arg)
                           (display #\space)
                           (write (syntax->datum arg)))
                         (action-args node))
               (display ")\"];")
               (newline)))))
    (display "}")
    (newline)))
