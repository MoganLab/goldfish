(library (extensible-match patterns)
  (export define-pattern-syntax match-ellipsis?
          and or not ? => quote
          seq seq* seq/unordered seq-aux
          cons cons* lset list vector eof-object
          quasiquote unquote unquote-splicing)
  (import (rnrs (6))
          (only (srfi :1 lists) append-map)
          (extensible-match pattern-syntax)
          (extensible-match core-pattern)
          (extensible-match expand)
          (extensible-match helpers))

  ;; Basic pattern syntax
  (define-pattern-syntax and
    (syntax-rules ()
      ((_) _)
      ((_ subpat) subpat)
      ((_ subpat more-subpats ...)
       (core:and subpat (and more-subpats ...)))))

  (define-pattern-syntax or
    (syntax-rules ()
      ((_) (core:not _))
      ((_ subpat) subpat)
      ((_ subpat more-subpats ...)
       (core:or subpat (or more-subpats ...)))))

  (define-pattern-syntax not
    (syntax-rules ()
      ((_ subpat) (core:not subpat))))

  (define-syntax ? (syntax-rules ()))
  (define-pattern-syntax ?
    (syntax-rules ()
      ((_ proc) (core:? proc))
      ((_ proc subpat ...)
       (core:and (core:? proc) (core:row subpat ...)))))

  (define-pattern-syntax quote
    (syntax-rules ()
      ((_ val) (core:quote val))))

  (define-pattern-syntax =>
    (lambda (stx)
      (syntax-case stx ()
        ((_ proc subpat ...)
         (with-syntax (((var ...) (generate-temporaries #'(subpat ...))))
           #'(core:apply proc
                         (var ...)
                         (core:row
                          (core:subject var subpat) ...)))))))

  (define-syntax seq (syntax-rules ()))
  (define-pattern-syntax seq
    (syntax-rules ()
      ((_ name ((var init step) ...) terminate? ref subpat ...)
       (seq-aux core/seq:ordered
                name
                ((var init step) ...)
                terminate? ref
                subpat ...))))
  (define-syntax seq* (syntax-rules ()))
  (define-pattern-syntax seq*
    (lambda (stx)
      (syntax-case stx ()
        ((_ name ((var init step) ...) terminate? ref subpat ... tail-subpat)
         (not (match-ellipsis? #'tail-subpat))
         #'(seq-aux core/seq:partial
                    name
                    ((var init step) ...)
                    terminate? ref
                    subpat ... tail-subpat)))))

  (define-syntax seq-aux (syntax-rules ()))
  (define-pattern-syntax seq-aux
    (lambda (stx)
      (syntax-case stx ()
        ((_ kind name ((var init step) ...) terminate? ref subpat ...)
         #`(core:seq
            kind name
            ((var init step) ...) terminate? ref
            #,@(let loop ((core-subpats '())
                          (more #'(subpat ...)))
                 (syntax-case more ()
                   (()
                    (reverse core-subpats))
                   ((subpat (ell n) more ...)
                    (and (identifier? #'ell)
                         (free-identifier=? #'ell #'(... ...))
                         (integer? (syntax->datum #'n))
                         (exact? (syntax->datum #'n)))
                    (loop (cons #'(core/seq:many n n subpat) core-subpats)
                          #'(more ...)))
                   ((subpat (ell min max) more ...)
                    (and (identifier? #'ell)
                         (free-identifier=? #'ell #'(... ...))
                         (integer? (syntax->datum #'min))
                         (exact? (syntax->datum #'min))
                         (or (eq? (syntax->datum #'max) #t)
                             (and (integer? (syntax->datum #'max))
                                  (exact? (syntax->datum #'max)))))
                    (loop (cons #'(core/seq:many min max subpat)
                                core-subpats)
                          #'(more ...)))
                   ((subpat ell more ...)
                    (and (identifier? #'ell)
                         (free-identifier=? #'ell #'(... ...)))
                    (loop (cons #'(core/seq:many 0 #t subpat) core-subpats)
                          #'(more ...)))
                   ((ell more ...)
                    (and (identifier? #'ell)
                         (free-identifier=? #'ell #'(... ...)))
                    (syntax-violation 'match
                                      "incorrect use of ellipsis"
                                      #'(subpat ...)
                                      #'ell))
                   (((ell . after-ell) more ...)
                    (and (identifier? #'ell)
                         (free-identifier=? #'ell #'(... ...)))
                    (syntax-violation 'match
                                      "incorrect use of ellipsis"
                                      #'(subpat ...)
                                      #'(ell . after-ell)))
                   ((subpat more ...)
                    (loop (cons #'(core/seq:one subpat) core-subpats)
                          #'(more ...))))))))))

  (define-syntax seq/unordered (syntax-rules ()))

  (define-pattern-syntax seq/unordered
    (lambda (stx)
      (let ((check-ellipsis
             (lambda (subpats)
               (cond ((find match-ellipsis? subpats)
                      => (lambda (ell)
                           (syntax-violation 'match
                                             "ellipsis is not allowed in unordered sequence patterns except for a final rest pattern"
                                             subpats
                                             ell)))))))
        (syntax-case stx ()
          ((_ name ((var init step) ...) terminate? ref subpat ...
              rest-subpat ell)
           (and (identifier? #'ell)
                (free-identifier=? #'ell #'(... ...)))
           (begin
             (check-ellipsis #'(subpat ...))
             #'(core:seq core/seq:unordered
                         name
                         ((var init step) ...) terminate? ref
                         (core/seq:many 0 #t rest-subpat)
                         (core/seq:one subpat) ...)))
          ((_ name ((var init step) ...) terminate? ref subpat ...)
           (begin
             (check-ellipsis #'(subpat ...))
             #'(core:seq core/seq:unordered
                         name
                         ((var init step) ...) terminate? ref
                         (core/seq:one subpat) ...)))))))

  (define-pattern-syntax cons
    (syntax-rules ()
      ((_ car-pat cdr-pat)
       (? pair?
          (=> car car-pat)
          (=> cdr cdr-pat)))))

  (define-pattern-syntax cons*
    (lambda (stx)
      ;; In the first stage of cons* expansion we recognize a prefix
      ;; without ellipses, as an optimization
      (syntax-case stx ()
        ((_ subpat (ell . after-ell) more-subpats ...)
         (and (identifier? #'ell)
              (free-identifier=? #'ell #'(... ...)))
         #'(cons*/ellipsis subpat (ell . after-ell) more-subpats ...))
        ((_ subpat ell more-subpats ...)
         (and (identifier? #'ell)
              (free-identifier=? #'ell #'(... ...)))
         #'(cons*/ellipsis subpat ell more-subpats ...))
        ((_ subpat_0 subpat_1 more-subpats ...)
         #'(cons subpat_0 (cons* subpat_1 more-subpats ...)))
        ((_ subpat_0)
         #'subpat_0))))

  (define-syntax cons*/ellipsis (syntax-rules ()))
  (define-pattern-syntax cons*/ellipsis
    (lambda (stx)
      (syntax-case stx ()
        ((_ subpat ... tail-subpat)
         #`(seq* ls ((curr ls (cdr curr)))
                 (not (pair? curr))
                 curr
             #,@(map
                 (lambda (subpat)
                   (if (match-ellipsis? subpat)
                       subpat
                       #`(=> car #,subpat)))
                 #'(subpat ...))
             tail-subpat)))))

  (define-pattern-syntax list
    (syntax-rules ()
      ((_ subpat ...)
       (cons* subpat ... '()))))

  (define-pattern-syntax vector
    (syntax-rules ()
      ((_ subpat ...)
       (and (? vector?)
            (seq vec ((idx 0 (+ idx 1)))
                 (>= idx (vector-length vec))
                 (vector-ref vec idx)
              subpat ...)))))

  (define-syntax lset (syntax-rules ()))
  (define-pattern-syntax lset
    (syntax-rules ()
      ((_ subpat ...)
       (and (? list?)
            (seq/unordered ls ((more ls (cdr more)))
                           (null? more)
                           (car more)
              subpat ...)))))

  (define-pattern-syntax eof-object
    (syntax-rules ()
      ((_) (? eof-object?))))

  (define-pattern-syntax quasiquote
    (lambda (stx)
      (define (expand-seq x d)
        (assert (>= d 0))
        (syntax-case x (quasiquote unquote unquote-splicing)
          ((unquote) (= d 0) '())
          ((unquote x) (= d 0) #'(x))
          ((unquote x ...)
           (= d 0)
           (append-map (lambda (x) (expand-seq x d))
                       #'((unquote x) ...)))
          ((unquote-splicing) (= d 0) '())
          ((unquote-splicing x)
           (= d 0)
           (if (identifier? #'x)
               #'(x (... ...))
               (syntax-violation 'unquote-splicing
                                 "only identifiers can be used with unquote-splicing"
                                 #'x)))
          ((unquote-splicing x ...)
           (= d 0)
           (append-map (lambda (x) (expand-seq x d))
                       #'((unquote-splicing x) ...)))
          ((uq qpat ...)
           (and (> d 0)
                (identifier? #'uq)
                (free-identifier=? #'uq #'unquote))
           (list #`(list 'uq #,@(append-map (lambda (x) (expand-seq x (- d 1)))
                                            #'(qpat ...)))))
          ((uqs qpat ...)
           (and (> d 0)
                (identifier? #'uqs)
                (free-identifier=? #'uqs #'unquote-splicing))
           (list #`(list 'uqs #,@(append-map (lambda (x) (expand-seq x (- d 1)))
                                             #'(qpat ...)))))
          ((qq x)
           (and (identifier? #'qq)
                (free-identifier=? #'qq #'quasiquote))
           (list #`(list 'qq #,(expand-quasiquote #'x (+ d 1)))))
          (ell
           (and (identifier? #'ell)
                (free-identifier=? #'ell #'(... ...)))
           #'(ell))
          ((ell . more)
           (and (identifier? #'ell)
                (free-identifier=? #'ell #'(... ...)))
           #'((ell . more)))
          (x (list (expand-quasiquote #'x d)))))
      (define (expand-quasiquote x d)
        (assert (>= d 0))
        (syntax-case x (quasiquote unquote unquote-splicing)
          ((qq qpat)
           (and (identifier? #'qq)
                (free-identifier=? #'qq #'quasiquote))
           #`(list 'qq #,(expand-quasiquote #'qpat (+ d 1))))
          ((unquote pat)
           (= d 0)
           #'pat)
          ((unquote pat ...)
           (= d 0)
           (syntax-violation 'quasiquote
                             "multi-subform unquote pattern used outside splicing context"
                             x))
          ((uq qpat ...)
           (and (> d 0)
                (identifier? #'uq)
                (free-identifier=? #'uq #'unquote))
           #`(list 'uq #,@(map (lambda (qsubpat)
                                 (expand-quasiquote qsubpat (- d 1)))
                               #'(qpat ...))))
          ((unquote-splicing qpat ...)
           (= d 0)
           (syntax-violation 'quasiquote
                             "multi-subform unquote pattern used outside splicing context"
                             x))
          ((uqs qpat ...)
           (and (> d 0)
                (identifier? #'uqs)
                (free-identifier=? #'uqs #'unquote-splicing))
           #`(list 'uqs #,@(map (lambda (qsubpat)
                                  (expand-quasiquote qsubpat (- d 1)))
                                #'(qpat ...))))
          (ell
           (and (identifier? #'ell)
                (free-identifier=? #'ell #'(... ...)))
           (syntax-violation 'quasiquote
                             "ellipsis used in pattern outside of splicing context"
                             stx))
          ((ell . more)
           (and (identifier? #'ell)
                (free-identifier=? #'ell #'(... ...)))
           (syntax-violation 'quasiquote
                             "ellipsis used in pattern outside of splicing context"
                             stx))
          ((x ... unquote y)
           (= d 0)
           #`(cons* #,@(append-map (lambda (x) (expand-seq x d)) #'(x ...)) y))
          ((x ... unquote-splicing y)
           (= d 0)
           #`(list #,@(append-map (lambda (x) (expand-seq x d)) #'(x ...)) y (... ...)))
          ((x_0 x_1 ... . y)
           #`(cons* #,@(append-map (lambda (x) (expand-seq x d)) #'(x_0 x_1 ...)) (quasiquote y)))
          (#(x ...)
           #`(vector #,@(append-map (lambda (x) (expand-seq x d)) #'(x ...))))
          (x #'(quote x))))
      (syntax-case stx ()
        ((_ x) (expand-quasiquote #'x 0))))))
