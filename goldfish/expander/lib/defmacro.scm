;;; defmacro.scm
;;; s7 define-macro compatibility layer, implemented with syntax-case
;;; (cf. Guile's ice-9/boot-9.scm defmacros).  A defmacro is a
;;; NON-HYGIENIC macro: its transformer is an ordinary procedure applied
;;; to the DATUM argument list of the macro call, and the datum it
;;; returns is re-injected at the use site.
;;;
;;;   (define-macro (macro-name . params) body ...)
;;;     == (define-syntax macro-name
;;;          (lambda (y) (datum->syntax y (apply (lambda params body ...)
;;;                                              (syntax->datum (cdr (syntax-e y)))))))
;;;
;;; This lets existing s7 define-macro libraries load through the
;;; expander without the s7 host macro engine.  It is a compatibility
;;; shim: new code should use define-syntax / syntax-rules / syntax-case.
;;;
;;; Installed by install.scm after syntax-case (which its expansion
;;; depends on).
;;;
;;; The definition is built with datum->syntax from the macro's own
;;; datum (no dotted template patterns), so it does not depend on
;;; dotted-pattern-variable instantiation.

(define-syntax define-macro
  (lambda (x)
    (syntax-case x ()
      ((_ (macro . params) body ...)
       (let* ((name (syntax->datum #'macro))
              (params-datum (syntax->datum #'params))
              (body-datums (syntax->datum #'(body ...)))
              (transformer `(lambda ,params-datum ,@body-datums)))
         (datum->syntax
          x
          (list 'define-macro name transformer))))
      ((_ macro transformer)
       #'(define-syntax macro
           (lambda (y)
             (datum->syntax
              y
              (apply transformer
                     (syntax->datum (cdr (syntax-e y)))))))))))

;;; defmacro : legacy alias with the old lispy defun syntax:
;;;   (defmacro name (args ...) body ...)
;;; which desugars to the define-macro form above.

(define-syntax defmacro
  (lambda (x)
    (syntax-case x ()
      ((_ macro (arg ...) body ...)
       #'(define-macro macro
           (lambda (arg ...) body ...))))))
