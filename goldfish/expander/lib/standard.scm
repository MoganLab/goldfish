;;; standard.scm
;;; R7RS derived forms that depend on runtime primitives beyond the
;;; minimal core (memv, make-promise, call-with-values), which resolve
;;; as free identifiers against the host.  The core derived forms
;;; (let/let*/and/or/cond/when/unless) live in core-macros.scm and are
;;; always installed; this file is optional:
;;;
;;;   (install-library-file! the-base-library "lib/standard.scm")
;;;   -- or simply (install-standard-library!) --
;;;
;;; Like core-macros.scm, this file is ORDINARY OBJECT-LEVEL R7RS
;;; SOURCE expanded by the expander itself; it uses no
;;; expander-internal API.

(define-syntax do
  (syntax-rules ()
    ((do ((var init step ...) ...)
         (test expr ...)
         command ...)
     (let loop ((var init) ...)
       (if test
           (begin expr ...)
           (begin command ...
                  (loop (do-step var step ...) ...)))))))

(define-syntax do-step
  (syntax-rules ()
    ((do-step var) var)
    ((do-step var step) step)))

(define-syntax delay
  (syntax-rules ()
    ((delay expr)
     (make-lazy-promise (lambda () expr)))))

(define-syntax delay-force
  (syntax-rules ()
    ((delay-force expr)
     (make-lazy-promise (lambda () (force expr))))))

(define-syntax let-values
  (syntax-rules ()
    ((let-values () body ...)
     (let () body ...))
    ((let-values ((formals expr)) body ...)
     (call-with-values (lambda () expr)
       (lambda formals body ...)))
    ((let-values ((formals expr) more ...) body ...)
     (call-with-values (lambda () expr)
       (lambda formals
         (let-values (more ...) body ...))))))

(define-syntax let*-values
  (syntax-rules ()
    ((let*-values () body ...)
     (let () body ...))
    ((let*-values ((formals expr)) body ...)
     (call-with-values (lambda () expr)
       (lambda formals body ...)))
    ((let*-values ((formals expr) more ...) body ...)
     (call-with-values (lambda () expr)
       (lambda formals
         (let*-values (more ...) body ...))))))

(define-syntax parameterize
  (syntax-rules ()
    ((parameterize () body1 body2 ...)
     (let () body1 body2 ...))
    ((parameterize ((param value) binding ...) body1 body2 ...)
     (let ((old (param)))
       (dynamic-wind
         (lambda () (param value))
         (lambda () (parameterize (binding ...) body1 body2 ...))
         (lambda () (param old)))))))

;;; case : procedural transformer.  Folds the clauses into a single
;;; (let ((k key)) (if ...)) chain in ONE expansion -- the recursive
;;; syntax-rules form was O(n^3) in the eager scope machinery.  Datums are
;;; quoted as literals compared with memv; the (d ...) => proc clause
;;; applies proc to the matching datum; else must be the last clause.
(define-syntax case
  (lambda (whole-expr)
    (letrec* ((form (syntax-form whole-expr))
              (def-stx (syntax (list)))
              (def-ctx (syntax-context def-stx))
              (def-lib (syntax-library def-stx))
              (else-id (datum->syntax whole-expr 'else))
              (arrow-id (datum->syntax whole-expr '=>))
              (k (make-syntax (make-fresh-name 'k) def-ctx def-lib))
              (key (cadr form))
              (emit-body
               (lambda (results)
    (if (null? results)
        (datum->syntax def-stx '(if #f #f))
        (if (null? (cdr results))
            (car results)
            (datum->syntax def-stx (cons 'begin results))))))
              (build
               (lambda (clauses)
                 (if (null? clauses)
        (datum->syntax def-stx '(if #f #f))
        (letrec* ((cl (car clauses))
                  (clf (syntax-e cl))
                  (datums (car clf))
                  (rest (cdr clf))
                  (rest-form (syntax-form rest))
                  (tail (build (cdr clauses)))
                  (else? (and (identifier? datums)
                              (free-identifier=? datums else-id)))
                  (memv-test (list 'memv k
                                   (list 'quote
                                         (syntax->datum (syntax-form datums)))))
                  (arrow (and (pair? rest-form)
                              (identifier? (car rest-form))
                              (free-identifier=? (car rest-form) arrow-id))))
          (if else?
              (emit-body rest-form)
              (if arrow
                  (letrec* ((t (make-syntax (make-fresh-name 't) def-ctx def-lib)))
    (datum->syntax def-stx
      (list 'let (list (list t memv-test))
            (list 'if t
                  (list (cadr rest-form)
                        (list 'car t))
                  tail))))
                  (datum->syntax def-stx
                    (list 'if memv-test
                          (emit-body rest-form)
                          tail)))))))))
      (letrec* ((body (build (cddr form))))
        (datum->syntax def-stx
          (list 'let (list (list k key)) body))))))
