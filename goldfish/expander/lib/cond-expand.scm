;;; lib/cond-expand.scm
;;; cond-expand (R7RS conditional expansion), objectified: an ordinary
;;; self-hosted macro expanded by the expander itself, instead of a kernel
;;; procedural form.  It only needs the expand-time syntax API
;;; (syntax-form / datum->syntax / syntax->datum), which any user-space
;;; transformer has.  Feature requirements are evaluated at expand time;
;;; the body of the first satisfied clause is spliced in as a begin.
;;; Feature set: r7rs + the implementation name.  (library ...)
;;; requirements are not yet checked and report unsatisfied.
;;;
;;; Installed after lib/core-macros.scm (so let / and / or / cond are
;;; available), matching the previous kernel boot order.

(define *cond-expand-features* '(r7rs scsyntax-impl))

(define (cond-expand-feature-satisfied? req)
  (let ((form (syntax-form req)))
    (cond
      ((symbol? form) (and (memq form *cond-expand-features*) #t))
      ((not (pair? form)) #f)
      (else
       (let ((head (syntax-form (car form))))
         (cond
           ((eq? head 'and)
            (let loop ((rs (cdr form)))
              (or (null? rs)
                  (and (cond-expand-feature-satisfied? (car rs))
                       (loop (cdr rs))))))
           ((eq? head 'or)
            (let loop ((rs (cdr form)))
              (and (not (null? rs))
                   (or (cond-expand-feature-satisfied? (car rs))
                       (loop (cdr rs))))))
           ((eq? head 'not)
            (not (cond-expand-feature-satisfied? (cadr form))))
           (else #f)))))))

(define-syntax cond-expand
  (lambda (stx)
    (syntax-case stx (else)
      ((_) (error "cond-expand: no matching feature requirement"
                 (syntax->datum stx)))
      ((_ (else body ...))
       #'(begin body ...))
      ((_ (feature body ...) rest ...)
       (if (cond-expand-feature-satisfied? #'feature)
           #'(begin body ...)
           #'(cond-expand rest ...))))))