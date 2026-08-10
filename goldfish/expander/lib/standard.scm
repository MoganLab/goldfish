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
     (let-values ((formals expr)) body ...))
    ((let*-values ((formals expr) more ...) body ...)
     (call-with-values (lambda () expr)
       (lambda formals
         (let*-values (more ...) body ...))))))

(define-syntax case
  (syntax-rules (else)
    ((case key)
     (begin key (if #f #f)))
    ((case key (else result ...))
     (begin key (begin result ...)))
    ((case key ((datum ...) result ...) clause ...)
     (let ((k key))
       (if (memv k (quote (datum ...)))
           (begin result ...)
           (case k clause ...))))))
