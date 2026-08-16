(library (extensible-match seq-pattern unordered)
  (export seq/unordered-pattern-expand)
  (import (rnrs (6))
          (only (srfi :1 lists) iota reverse!)
          (extensible-match ast)
          (extensible-match seq-pattern common)
          (extensible-match util))

  (define (seq/unordered-pattern-expand seq-pattern expand-subpat
                                        conseq alter)
    (let*-values
        ;; The first stage is common to all seq pattern
        ;; strategies; see the commentary in common.sls
        (((test-defs test-patterns)
          (seq-pattern->defs+test-patterns seq-pattern
                                           expand-subpat))
         ((action-defs action-proc-ids rest-proc-id)
          (test-patterns->action-procedures test-patterns))
         ((all-vars list-vars)
          (test-pattern-variables test-patterns))
         ((n-registers n-tests)
          (values (length all-vars)
                  (if rest-proc-id
                      (- (length test-patterns) 1)
                      (length test-patterns))))
         ((all-matched-bits)
          (- (bitwise-arithmetic-shift 1 n-tests) 1)))
      (with-syntax
          ((((state-var init step) ...)
            (map (lambda (state-var)
                   (list (seq-state-var-name state-var)
                         (seq-state-var-init state-var)
                         (seq-state-var-step state-var)))
                 (seq-pattern-state-vars seq-pattern)))
           ((var ...) all-vars)
           ((list-var ...) list-vars)
           ((var-reg ...) (iota n-registers)))
        #`(let ()
            (define (success var ...) #,conseq)
            (define (failure) #,alter)
            (let ()
              #,test-defs
              #,action-defs
              (define action-procs (vector #,@action-proc-ids))
              (let ()
                (define #,(seq-pattern-name seq-pattern)
                  #,(pattern-subject seq-pattern))
                (let loop ((state-var init) ...
                           (backtracking-point #f)
                           (registers (make-registers #,n-registers))
                           (matched-patterns 0))
                  (if #,(seq-pattern-termination-expr seq-pattern)
                      (if (eqv? matched-patterns #,all-matched-bits)
                          (let ((var (register-ref registers var-reg)) ...)
                            (let ((list-var (reverse! list-var)) ...)
                              (success var ...)))
                          (failure))
                      (let ((current-value #,(seq-pattern-ref-expr seq-pattern)))
                        (let pattern-loop ((idx 0))
                          (cond ((>= idx (vector-length action-procs))
                                 #,(if rest-proc-id
                                       #`(cond
                                          ((#,rest-proc-id current-value
                                                           registers)
                                           => (lambda (new-registers)
                                                (loop step ...
                                                      backtracking-point
                                                      new-registers
                                                      matched-patterns)))
                                          ((not backtracking-point)
                                           (failure))
                                          (else
                                           (backtracking-point)))
                                       #`(if backtracking-point
                                             (backtracking-point)
                                             (failure))))
                                ((bitwise-bit-set? matched-patterns idx)
                                 (pattern-loop (+ idx 1)))
                                (((vector-ref action-procs idx)
                                  current-value
                                  registers)
                                 => (lambda (new-registers)
                                      (registers-cow! registers)
                                      (loop step ...
                                            (lambda ()
                                              (pattern-loop (+ idx 1)))
                                            new-registers
                                            (bitwise-bit-set matched-patterns idx))))
                                (else
                                 (pattern-loop (+ idx 1))))))))))))))

  (define (test-patterns->action-procedures test-patterns)
    (let loop ((more test-patterns)
               (register-offset 0)
               (defs '())
               (ids '())
               (rest-proc-id #f))
      (if (null? more)
          (values #`(begin #,@defs)
                  (reverse ids)
                  rest-proc-id)
          (syntax-case (car more) (test:one test:many)
            ((test:one (var ...) test-proc-id)
             (let ((id (car (generate-temporaries '(t)))))
               (loop (cdr more)
                     (+ register-offset (length #'(var ...)))
                     (cons #`(define #,id
                               #,(one-action-procedure #'test-proc-id
                                                       (length #'(var ...))
                                                       register-offset))
                           defs)
                     (cons id ids)
                     rest-proc-id)))
            ((test:many (var ...) 0 #t test-proc-id)
             (let ((id (car (generate-temporaries '(t)))))
               (loop (cdr more)
                     (+ register-offset (length #'(var ...)))
                     (cons #`(define #,id
                               #,(many-action-procedure #'test-proc-id
                                                        (length #'(var ...))
                                                        register-offset))
                           defs)
                     ids
                     id)))
            ((test:many (_ ...) _ _ _)
             (syntax-violation 'seq/unordered
                               "seq/unordered may only have an unbounded rest pattern"
                               #f #f)))))))
