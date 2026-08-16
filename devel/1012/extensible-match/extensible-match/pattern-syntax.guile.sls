(library (extensible-match pattern-syntax)
  (export define-pattern-syntax
          expand-pattern-syntax)
  (import (guile)
          (only (rnrs lists) exists find memp)
          (only (rnrs base) let-values)
          (only (rnrs eval) environment)
          (system syntax))

  (define *pattern-syntax* (make-hash-table))

  (define make-binding list)
  (define binding-keyword car)
  (define binding-secret cadr)
  (define binding-tx caddr)
  (define (set-pattern-syntax! mod id secret tx)
    (let ((binding (make-binding id secret tx)))
      (cond ((hash-get-handle *pattern-syntax* mod)
             => (lambda (handle)
                  (set-cdr! handle
                            (cons binding (cdr handle)))))
            (else (hash-set! *pattern-syntax* mod (list binding))))))

  (define-syntax define-pattern-syntax
    (lambda (stx)
      (syntax-case stx ()
        ((_ for-id proc-expr)
         (with-syntax (((secret) (generate-temporaries '(blah))))
           #'(begin
               (define secret #t)
               (%define-pattern-syntax secret for-id proc-expr)))))))
  (define-syntax %define-pattern-syntax
    (lambda (stx)
      (syntax-case stx ()
        ((_ secret for-id proc-expr)
         (with-syntax (((internal-name) (generate-temporaries '(blah))))
           (let-values (((type val) (syntax-local-binding #'secret)))
             ;; This trick detects whether the definition is at module
             ;; top-level or internal to some block
             (if (eq? type 'global)
                 ;; Case for top-level pattern syntax has to run
                 ;; `set-pattern-syntax!' when the compiled module is
                 ;; loaded as well as when run interactively. This is
                 ;; apparently sufficient, no eval-when needed.
                 #'(define-syntax internal-name
                     (let ((proc proc-expr))
                       (set-pattern-syntax! (module-name (current-module))
                                            (quote-syntax for-id)
                                            #f
                                            (quote-syntax internal-name))
                       proc))
                 ;; Case for block-level pattern syntax sets the
                 ;; secret so we can check that it’s in scope when
                 ;; looking up later. TODO: work out why eval-when
                 ;; didn’t work here to stop set-pattern-syntax! being
                 ;; compiled, since in local contexts the transformer
                 ;; doesn’t need to be saved.
                 #'(define-syntax internal-name
                     (let ((proc proc-expr))
                       (set-pattern-syntax! (module-name (current-module))
                                            (quote-syntax for-id)
                                            (quote-syntax secret)
                                            (quote-syntax internal-name))
                       proc)))))))))

  (define-syntax expand-pattern-syntax
    (lambda (stx)
      ;; some banal utilities
      (define quote-syntax-rename (make-symbol "qs"))
      (define qs-module (environment `(rename (only (guile) quote-syntax)
                                              (quote-syntax ,quote-syntax-rename))))
      ;; performance hack: memoize this fairly expensive procedure
      (define exports?-memo (make-hash-table))
      (define (%exports? mod id)
        (call/cc
         (lambda (ret)
           (let ((module-with-qs (make-module)))
             (module-use! module-with-qs mod)
             (module-use! module-with-qs qs-module)
             (hash-for-each
              (lambda (name value)
                (if (free-identifier=? id
                                       (eval `(,quote-syntax-rename ,name)
                                             module-with-qs))
                    (ret #t)))
              (module-obarray mod)))
           #f)))
      (define (exports? mod id)
        (cond ((hashq-ref exports?-memo mod #f) =>
               (lambda (id-table)
                 (let ((maybe-res (hashq-ref id-table id 'nothing)))
                   (if (eq? maybe-res 'nothing)
                       (let ((res (%exports? mod id)))
                         (hashq-set! id-table id res)
                         res)
                       maybe-res))))
              (else
               (let ((id-table (make-hash-table)))
                 (hashq-set! exports?-memo mod id-table)
                 (let ((res (%exports? mod id)))
                   (hashq-set! id-table id res)
                   res)))))

      (define (find-patstx-binding mod keyword)
        (find (lambda (binding)
                (and (free-identifier=? (binding-keyword binding) keyword)
                     (or (not (binding-secret binding))
                         (memp (lambda (secret)
                                 (free-identifier=? (binding-secret binding)
                                                    secret))
                               (syntax-locally-bound-identifiers keyword)))))
              (hash-ref *pattern-syntax* (module-name mod) '())))

      (define imports-memo (make-hash-table))
      (define (find-imported-patstx-binding imports keyword)
        (let loop ((more-imports imports))
          (if (null? more-imports) #f
              (let ((imported-module (car more-imports)))
                (if (hashq-get-handle imports-memo imported-module)
                    (loop (cdr more-imports))
                    (begin
                      (hashq-set! imports-memo imported-module #t)
                      (cond
                       ((or (find-patstx-binding imported-module keyword)
                            (find-imported-patstx-binding
                             (module-uses (resolve-module (module-name imported-module)))
                             keyword))
                        => (lambda (maybe-binding)
                             (if (exports? imported-module keyword)
                                 maybe-binding
                                 (loop (cdr more-imports)))))
                       (else
                        (loop (cdr more-imports))))))))))
      (define (binding->proc binding)
        (let-values (((type proc) (syntax-local-binding
                                   (binding-tx binding))))
          proc))
      (define (lookup keyword)
        (let ((use-mod (resolve-module (syntax-module keyword))))
          (cond ((find-patstx-binding use-mod keyword)
                 => binding->proc)
                ((find-imported-patstx-binding (module-uses use-mod)
                                               keyword)
                 => binding->proc)
                (else #f))))

      (syntax-case stx ()
        ((_ (patstx-keyword . rest) k-keyword . k-subforms)
         (cond ((lookup #'patstx-keyword)
                => (lambda (tx)
                     #`(k-keyword #,(tx #'(patstx-keyword . rest)) . k-subforms)))
               (else (syntax-violation 'match
                                       "pattern syntax not defined"
                                       #'patstx-keyword))))))))
