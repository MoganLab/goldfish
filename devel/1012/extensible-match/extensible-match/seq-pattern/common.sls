(library (extensible-match seq-pattern common)
  (export test:one test:many
          seq-pattern->defs+test-patterns
          test-pattern-variables

          make-registers
          registers-cow!
          registers-set!
          register-ref

          one-action-procedure
          many-action-procedure)
  (import (rnrs (6))
          (only (srfi :1 lists) append-reverse iota)
          (extensible-match util)
          (extensible-match ast))

  ;; In the first stage for all the strategies, we convert the
  ;; subpatterns into a series of procedure definitions; each
  ;; procedure corresponds to one subpattern and does the actual job
  ;; of matching that subpattern. This is advantageous especially for
  ;; the NFA implementation, because the same matcher may be used for
  ;; multiple instructions which all correspond to one subpattern in
  ;; the original list pattern; splitting the patterns out into
  ;; matching procedures earlier reduces our dependency on the Scheme
  ;; compiler to recognize that two lambda expressions are the same.
  ;; However, since all of the strategies depend on some kind of
  ;; conversion of sequence subpatterns to code, we do this here.
  ;;
  ;; We use multi-value return from these procedures as a cheap and
  ;; cheerful option type: zero values means no match, at least one
  ;; value means a match. The first value is always #t, so we can
  ;; disambiguate between unsuccessful matches and successful matches
  ;; that bind no pattern variables.
  ;;
  ;; test:x takes the place of core/seq:x in the output list; the
  ;; difference is that the subpattern is the name of matcher
  ;; procedure and a new subform is added at the start, listing the
  ;; variables that were in the subpattern.
  ;;
  ;; test-pattern ::= (test:one vars name)
  ;;               |  (test:many vars min max name)
  ;;
  ;; name is the generated identifier bound to the test procedure for
  ;; that test pattern
  (define-syntax test:one (syntax-rules ()))
  (define-syntax test:many (syntax-rules ()))

  (define (seq-subpat-success-action . args) (apply values #t args))
  (define (seq-subpat-failure-action) (values))

  (define (seq-pattern->defs+test-patterns seq-pattern expand-subpat)
    (define (test-proc-syntax subpat)
      #`(lambda (#,(seq-pattern-subpat-subject seq-pattern))
          #,(expand-subpat subpat
                           (make-action #'seq-subpat-success-action
                                        (pattern-vars subpat))
                           (make-action #'seq-subpat-failure-action
                                        '()))))
    (let ((test-pattern-data
           (map
            (lambda (seq-subpat)
              (let* ((vars (seq-pattern-vars seq-subpat))
                     (n-vars (length vars))
                     (name (car (generate-temporaries '(t)))))
                (cond ((seq-pattern-one? seq-subpat)
                       (list name
                             #`(test:one #,vars)
                             (test-proc-syntax
                              (seq-pattern-one-subpat seq-subpat))))
                      ((seq-pattern-many? seq-subpat)
                       (list name
                             #`(test:many #,vars
                                          #,(seq-pattern-many-min seq-subpat)
                                          #,(seq-pattern-many-max seq-subpat))
                             (test-proc-syntax
                              (seq-pattern-many-subpat seq-subpat)))))))
            (seq-pattern-subpats seq-pattern))))
      (syntax-case test-pattern-data ()
        (((name (test-form ...) proc) ...)
         (values #'(begin
                     (define name proc) ...)
                 #'((test-form ... name) ...))))))

  (define (test-pattern-variables test-patterns)
    (let loop ((all-vars '())
               (list-vars '())
               (more test-patterns))
      (if (null? more)
          (values (reverse all-vars)
                  (reverse list-vars))
          (syntax-case (car more) ()
            ((k (var ...) . _)
             (loop (append-reverse #'(var ...) all-vars)
                   (if (free-identifier=? #'k #'test:many)
                       (append-reverse #'(var ...) list-vars)
                       list-vars)
                   (cdr more)))))))

  ;; All the strategies use register sets to represent pattern
  ;; variables while progress is still being made through the
  ;; sequence. There is an efficiency trade-off to be made in terms of
  ;; the choice of data structure. As a stepping stone to potentially
  ;; using a radix tree in future, we mildly abstract here over a
  ;; vector; this means that, for any given pattern, the constant
  ;; factor by which the speed of matching an input sequence is
  ;; increased is quadratic. However, for a slight improvement on the
  ;; theoretical worst-case, the register set is copy-on-write so that
  ;; matching attempts which fail immediately don’t copy the vector
  ;; unnecessarily.
  ;;
  ;; For information on the benefits and disadvantages of various
  ;; representation strategies see section 4.6 of:
  ;;
  ;; A. Barrière et al. ‘Linear Matching of JavaScript Regular
  ;; Expressions’, Proc. ACM Program Lang. PLDI 8 (2024), art. 201.
  ;; <https://doi.org/10.1145/3656431>

  (define (make-registers size)
    (let ((vec (make-vector (+ size 1) '())))
      (vector-set! vec 0 0)
      vec))
  (define (registers-cow! regs)
    (vector-set! regs 0 (+ 1 (vector-ref regs 0))))
  (define (registers-set! regs . idxs+vals)
    (if (eqv? (vector-ref regs 0) 0)
        (begin
          (let loop ((more-idxs+vals idxs+vals))
            (unless (null? more-idxs+vals)
              (vector-set! regs
                           (+ 1 (car more-idxs+vals))
                           (cadr more-idxs+vals))
              (loop (cddr more-idxs+vals))))
          regs)
        (let ((new-regs (make-registers (- (vector-length regs) 1))))
          (let loop ((idx 1))
            (unless (>= idx (vector-length regs))
              (vector-set! new-regs idx (vector-ref regs idx))
              (loop (+ idx 1))))
          (vector-set! regs 0 (- (vector-ref regs 0) 1))
          (apply registers-set! new-regs idxs+vals))))
  (define (register-ref regs idx)
    (vector-ref regs (+ idx 1)))

  ;; An action procedure is a non-pure wrapper for a pure functional
  ;; test procedure. If the test procedure returns zero values, the
  ;; test fails; if the test procedure returns more than one value,
  ;; some registers are updated as appropriate. The action procedure
  ;; returns the updated register set if matching succeeded, or #f if
  ;; matching did not succeed.
  (define (one-action-procedure test-proc n-registers register-offset)
    (with*-syntax (((var ...) (generate-temporaries (iota n-registers)))
                   ((var-reg ...) (iota n-registers register-offset))
                   (((reg-set ...) ...)
                    ;; When matching a single-item pattern, we simply
                    ;; set the corresponding register to the matched
                    ;; value
                    #'((var-reg var) ...)))
      #`(lambda (input regs)
          (call-with-values
              (lambda ()
                (#,test-proc input))
            (case-lambda
              ((t var ...)
               (registers-set! regs reg-set ... ...))
              (() #f))))))

  (define (many-action-procedure test-proc n-registers register-offset)
    (with*-syntax (((var ...) (generate-temporaries (iota n-registers)))
                   ((var-reg ...) (iota n-registers register-offset))
                   (((reg-set ...) ...)
                    ;; When matching an ellipsized (‘many’) pattern,
                    ;; we cons the matched item onto the old value of
                    ;; the register; we will reverse the resulting
                    ;; list once the whole pattern is known to have
                    ;; matched.
                    #'((var-reg (cons var
                                      (register-ref regs var-reg))) ...)))
      #`(lambda (input regs)
          (call-with-values
              (lambda ()
                (#,test-proc input))
            (case-lambda
              ((t var ...)
               (registers-set! regs reg-set ... ...))
              (() #f)))))))
