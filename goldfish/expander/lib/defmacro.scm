;;; defmacro.scm
;;; s7 define-macro compatibility layer (cf. Guile's ice-9/boot-9.scm
;;; defmacros).  A defmacro is a NON-HYGIENIC macro: its transformer is an
;;; ordinary procedure applied to the DATUM argument list of the macro
;;; call, and the datum it returns is re-injected at the use site.
;;;
;;;   (define-macro (macro-name . params) body ...)
;;;
;;; The transformer is built from its datum (params/body) and EVALUATED
;;; directly with s7 (install-defmacro-transformer), NOT expanded by the
;;; expander: s7 defmacro bodies routinely use host features the expander
;;; treats as keywords or rejects as values -- e.g. `(apply lambda ...)' in
;;; (liii base)'s typed-lambda, or backquote with unquote-splicing.  s7
;;; eval (in the-expander-library, falling back to the rootlet) compiles
;;; those fine.  Only the (define-syntax ...) shell around it goes through
;;; the expander, so the macro still installs as a normal transformer.
;;;
;;; Installed by install.scm after syntax-case (which its expansion
;;; depends on).
;;;
;;; The definition is built with datum->syntax from the macro's own datum
;;; (no dotted template patterns), so it does not depend on
;;; dotted-pattern-variable instantiation.

(define (install-defmacro-transformer name params body)
  (eval (cons 'lambda (cons params body)) the-expander-library))

;; Register the helper under its bare name: lib-layer defines are
;; scope-renamed at install time (install-defmacro-transformer:0), so a
;; datum reference from generated transformer output would otherwise be
;; unbound.  module-define! keys on the ORIGINAL identifier (the define's
;; bind target), keeping the bare name resolvable at eval time.

(define %defmacro-api-exported!
  (module-define! the-expander-library 'install-defmacro-transformer
                  install-defmacro-transformer))

(define-syntax define-macro
  (lambda (x)
    (syntax-case x ()
      ((_ (macro . params) body ...)
       (let* ((name (syntax->datum #'macro))
              (params-datum (syntax->datum #'params))
              (body-datums (syntax->datum #'(body ...)))
              (closure
               (list 'install-defmacro-transformer
                     (list 'quote name)
                     (list 'quote params-datum)
                     (list 'quote body-datums))))
         (datum->syntax
          x
          (list 'define-syntax
                name
                (list 'lambda
                      (list 'y)
                      (list 'datum->syntax
                            'y
                            (list 'apply
                                  closure
                                  (list 'syntax->datum
                                        (list 'cdr (list 'syntax-e 'y))))))))))
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

;;; macro : s7's shorthand for define-macro:
;;;   (macro (name . params) body ...)  ==  (define-macro (name . params) body ...)
;;; Used in the wild by (liii case)'s case*, which builds its macro as the
;;; value of a let.  define-macro itself expands to a define-syntax, which
;;; is a definition -- so this alias must also work in definition position
;;; inside a body scan (the intdef scanner recognizes define-macro).

(define-syntax macro
  (lambda (x)
    (syntax-case x ()
      ((_ (name . params) body ...)
       #'(define-macro (name . params) body ...))
      ((_ name expr)
       #'(define-macro name expr)))))
