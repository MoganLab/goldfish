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

(define *cond-expand-features* '(r7rs goldfish))

(define (cond-expand-feature-satisfied? req)
  (let ((form (syntax-form req)))
    (if (symbol? form)
        (if (memq form *cond-expand-features*) #t #f)
        (if (not (pair? form))
            #f
            (let ((head (syntax-form (car form))))
              (if (eq? head 'and)
                  (let loop ((rs (cdr form)))
                    (if (null? rs)
                        #t
                        (if (cond-expand-feature-satisfied? (car rs))
                            (loop (cdr rs))
                            #f)))
                  (if (eq? head 'or)
                      (let loop ((rs (cdr form)))
                        (if (null? rs)
                            #f
                            (if (cond-expand-feature-satisfied? (car rs))
                                #t
                                (loop (cdr rs)))))
                      (if (eq? head 'not)
                          (not (cond-expand-feature-satisfied? (cadr form)))
                          #f))))))))

(define-syntax cond-expand
  (lambda (stx)
    (let ((clauses (cdr (syntax-form stx))))
      (let loop ((rest clauses))
        (if (null? rest)
            (error 'cond-expand "no matching feature requirement"
                   (syntax->datum stx))
            (let* ((clause (car rest))
                   (form (if (syntax? clause) (syntax-form clause) clause))
                   ;; Clause heads arrive wrapped (the form spine holds
                   ;; syntax objects); compare the unwrapped symbol or the
                   ;; else branch never matches and every cond-expand
                   ;; degrades to "no matching feature requirement".
                   (head-raw (if (pair? form) (car form) #f))
                   (head (if (syntax? head-raw)
                             (syntax-form head-raw)
                             head-raw)))
              (if (eq? head 'else)
                  (datum->syntax stx (cons 'begin (cdr form)))
                  (if (and (pair? form)
                           (cond-expand-feature-satisfied? (car form)))
                      (datum->syntax stx (cons 'begin (cdr form)))
                      (loop (cdr rest))))))))))
