;;; core-macros.scm
;;; Core derived forms, always installed into the-base-library.
;;;
;;; This file is ORDINARY OBJECT-LEVEL R7RS SOURCE: the driver reads it
;;; at boot and expands it with the expander itself
;;; (install-library-file!), exactly like any user program.  It uses no
;;; expander-internal API -- only define-syntax.
;;;
;;; These forms have no recursive binding of their own and derive to
;;; lambda / core forms, so they belong in user space rather than the
;;; expander's core-form table.  Recursive binding (letrec / letrec* /
;;; internal define) stays primitive -- see PLAN-core-module.md.
;;;
;;; syntax-case and cond-expand are NOT defined here: they are
;;; irreducibly procedural and installed by the kernel
;;; (boot/primitives.scm) before this file is expanded.

(define-syntax with-syntax
  (syntax-rules ()
    ((with-syntax ((pat expr) ...) body ...)
     (syntax-case (list expr ...) ()
       ((pat ...) (let () body ...))))))

(define-syntax let
  (syntax-rules ()
    ((let ((name val) ...) body ...)
     ((lambda (name ...) body ...) val ...))
    ((let tag ((name val) ...) body ...)
     ((letrec ((tag (lambda (name ...) body ...))) tag) val ...))))

(define-syntax let*
  (syntax-rules ()
    ((let* () body ...)
     (let () body ...))
    ((let* ((name val)) body ...)
     (let ((name val)) body ...))
    ((let* ((name val) rest ...) body ...)
     (let ((name val)) (let* (rest ...) body ...)))))

;;; letrec is a core form (core-forms.scm), emitted as-is so the host
;;; evaluates it with its strict R7RS letrec semantics (referencing an
;;; uninitialized binding in an init is an error), distinct from letrec*.
;;; letrec* stays the recursive-binding core form used for internal defines.

(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and e) e)
    ((and e1 e2 ...) (if e1 (and e2 ...) #f))))

(define-syntax or
  (syntax-rules ()
    ((or) #f)
    ((or e) e)
    ((or e1 e2 ...) (let ((t e1)) (if t t (or e2 ...))))))

(define-syntax cond
  (syntax-rules (else =>)
    ((cond) (if #f #f))
    ((cond (else result ...)) (begin result ...))
    ((cond (test => proc) clause ...)
     (let ((t test))
       (if t (proc t) (cond clause ...))))
    ((cond (test) clause ...)
     (let ((t test))
       (if t t (cond clause ...))))
    ((cond (test result ...) clause ...)
     (if test (begin result ...) (cond clause ...)))))

(define-syntax when
  (syntax-rules ()
    ((when test body ...)
     (if test
       (begin body ...)
       (if #f #f)))))

(define-syntax unless
  (syntax-rules ()
    ((unless test body ...)
     (if test
       (if #f #f)
       (begin body ...)))))
