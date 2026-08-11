;;; syntax-case.scm
;;; syntax-case as an ordinary object-level macro.  In Racket,
;;; syntax-case is derived -- a macro built on kernel primitives
;;; (datum->syntax, free-identifier=?, quote-syntax,
;;; letrec-syntaxes+values; collects/racket/private/stxloc.rkt), not a
;;; form the expander dispatches directly.  Here likewise: this file is
;;; expanded by the expander itself and installed into the-base-library
;;; by the driver.
;;;
;;; Boot order: the driver installs lib/syntax-runtime.scm first (the
;;; pattern-matching / instantiation / dispatch procedures this macro's
;;; output calls), then this file, then core-macros.scm -- because
;;; syntax-rules desugaring expands to (lambda (stx) (syntax-case stx ...))
;;; at phase+1 and needs syntax-case already bound.  This file is at the
;;; bottom of the bootstrap ladder: written in core forms only (lambda /
;;; if / begin / set! / quote / letrec*) -- no derived forms exist yet
;;; when it is expanded -- and using only the syntax-object primitives.
;;;
;;; The transformer compiles each clause to a run-time spec consumed by
;;; syntax-case-dispatch (expansion-time runtime, lib/syntax-runtime.scm):
;;; pattern matching stays in the dispatcher, and each (syntax T) in a
;;; fender/body is rewritten to an explicit
;;;   (instantiate (syntax T) (list (cons 'p p) ...))
;;; call so pattern variables are substituted at run time.

(define-syntax syntax-case
  (lambda (macro-stx)
    (letrec* ((spine-map
               (lambda (f xs)
                 (if (null? xs)
                     '()
                     (if (pair? xs)
                         (cons (f (car xs)) (spine-map f (cdr xs)))
                         (f xs)))))

              (pattern-variables
               (lambda (pat literal-ids)
                 (letrec* ((vars '())
                           (walk (lambda (p)
                                   (if (identifier? p)
                                       (let ((form (syntax-form p)))
                                         (if (if (eq? form '_)
                                                 #t
                                                 (if (eq? form '...)
                                                     #t
                                                     (if (and (identifier? p) (literal-id? p))
                                                         #t
                                                         (memq form vars))))
                                             (if #f #f)
                                             (set! vars (cons form vars))))
                                       (let ((form (if (syntax? p) (syntax-form p) p)))
                                         (if (pair? form)
                                             (begin (walk (car form))
                                                    (walk (cdr form)))
                                             (if (vector? form)
                                                 (for-each walk (vector->list form))
                                                 (if #f #f)))))))
                           (literal-id?
                            (lambda (p)
                              (let loop ((ls literal-ids))
                                (if (null? ls)
                                    #f
                                    (if (and (identifier? (car ls))
                                             (bound-identifier=? p (car ls)))
                                        #t
                                        (loop (cdr ls))))))))
                   (begin (walk pat)
                          (reverse vars)))))

              (build-instantiate-call
               (lambda (template patvars src-stx var-ctx)
                 (letrec* ((lib (syntax-library src-stx)))
                   (datum->syntax src-stx
                     (list 'instantiate
                           (list 'syntax template)
                           (cons 'list
                                 (map (lambda (p)
                                        (list 'cons
                                              (list 'quote p)
                                              (make-syntax p var-ctx lib)))
                                      patvars)))))))

              (generic-recurse
               (lambda (form stx patvars sctx lib)
                 (make-syntax (spine-map (lambda (sub)
                                           (transform-syntax-body sub patvars sctx lib))
                                         form)
                              (syntax-context stx) (syntax-library stx))))

              (transform-syntax-body
               (lambda (stx patvars sctx lib)
                 (letrec* ((form (syntax-form stx)))
                   (if (pair? form)
                       (if (identifier? (car form))
                           (letrec* ((head (syntax-form (car form))))
                             (if (eq? head 'syntax)
                                 (build-instantiate-call (cadr form) patvars stx sctx)
                                 (if (eq? head 'syntax-case)
                                     (make-syntax
                                      (cons (car form)
                                            (cons (transform-syntax-body (cadr form) patvars sctx lib)
                                                  (cons (caddr form) (cdddr form))))
                                      (syntax-context stx) (syntax-library stx))
                                     (if (eq? head 'with-syntax)
                                         (letrec* ((bindings-stx (cadr form))
                                                   (new-bindings
                                                    (map (lambda (b)
                                                           (letrec* ((bf (syntax-form b)))
                                                             (make-syntax
                                                              (list (car bf)
                                                                    (transform-syntax-body (cadr bf) patvars sctx lib))
                                                              (syntax-context b) (syntax-library b))))
                                                         (syntax-form bindings-stx))))
                                           (make-syntax
                                            (cons (car form)
                                                  (cons (make-syntax new-bindings
                                                                     (syntax-context bindings-stx)
                                                                     (syntax-library bindings-stx))
                                                        (cddr form)))
                                            (syntax-context stx) (syntax-library stx)))
                                         (generic-recurse form stx patvars sctx lib)))))
                           (generic-recurse form stx patvars sctx lib))
                       stx))))

              (compile-clause
               (lambda (clause-stx literal-ids sctx lib)
                 (letrec* ((clause (syntax-form clause-stx))
                           (pattern-stx (car clause))
                           (rest (cdr clause))
                           (fender-stx (if (= 2 (length rest)) (car rest) #f))
                           (body-stx (if (= 2 (length rest)) (cadr rest) (car rest)))
                           (patvars (pattern-variables pattern-stx literal-ids))
                           (body-xformed (transform-syntax-body body-stx patvars sctx lib))
                           (fender-xformed (if fender-stx
                                               (transform-syntax-body fender-stx patvars sctx lib)
                                               #t)))
                   (list 'list
                         (list 'syntax pattern-stx)
                         (list 'quote patvars)
                         (list 'lambda patvars fender-xformed)
                         (list 'lambda patvars body-xformed))))))

      (letrec* ((form (syntax-form macro-stx))
                (input-expr (cadr form))
                (literals-stx (caddr form))
                (clauses (cdddr form))
                (literal-ids (syntax-form literals-stx))
                (sctx (syntax-context macro-stx))
                (lib (syntax-library macro-stx)))
        (datum->syntax macro-stx
          (list 'syntax-case-dispatch
                input-expr
                (list (make-syntax 'syntax sctx lib)
                      (make-syntax literal-ids sctx lib))
                (cons 'list
                      (map (lambda (cl)
                             (compile-clause cl literal-ids sctx lib))
                           clauses))))))))

;;; syntax-rules : (syntax-rules (lit ...) (pat tmpl) ...) -> transformer
;;; A plain macro over syntax-case, mirroring Racket (syntax-rules is
;;; itself derived from syntax-case).  Each (pat tmpl) clause becomes a
;;; syntax-case clause (pat (syntax tmpl)); the generated transformer is
;;; (lambda (tmp) (syntax-case tmp (lit ...) ...)).  Previously this
;;; desugaring lived in the kernel seam (transformer.scm); moving it here
;;; leaves eval-transformer as the only eval seam.

(define-syntax syntax-rules
  (lambda (macro-stx)
    (syntax-case macro-stx ()
      ((syntax-rules (lit ...) ((keyword . pattern) template) ...)
       ;; Each rule's pattern is ((keyword . pattern) template): the keyword
       ;; is the pattern-keyword position (the macro name), matched against
       ;; the macro-use head.  The generated transformer replaces it with a
       ;; FRESH pattern variable `head' (make-fresh-name), so the head
       ;; matches any keyword without binding anything the user can
       ;; reference -- a template reference to the rule head is therefore a
       ;; free/unbound identifier (R7RS/Guile semantics; ((a) (a)) errors
       ;; instead of self-applying and looping).  A fresh variable, rather
       ;; than the `_' wildcard, is required because `_' is the match.scm
       ;; style literal-underscore marker: when the user lists `_' among the
       ;; literals, `_' in body positions must match only the identifier `_'
       ;; itself -- an `_' rule head would then stop matching real keywords.
       ;; The transformer is built programmatically (datum->syntax) so the
       ;; fresh head can be spliced into every clause pattern; its free
       ;; identifiers (lambda / syntax-case / syntax / tmp) sit in the
       ;; syntax-rules DEFINITION context, so they resolve to the base
       ;; library with bare references -- hygiene requires this, and it
       ;; avoids cross-library (module-ref (scsyntax) ...) refs.
        (letrec* ((def-stx (syntax (list)))
                  (def-ctx (syntax-context def-stx))
                  (def-lib (syntax-library def-stx))
                  (tmp (make-syntax (make-fresh-name 'tmp) def-ctx def-lib))
                  (head (make-syntax (make-fresh-name 'kw) def-ctx def-lib))
                  (rules (syntax-form (syntax (((keyword . pattern) template) ...)))))
          (datum->syntax def-stx
            (cons 'lambda
                  (cons (list tmp)
                        (list (cons 'syntax-case
                                    (cons tmp
                                          (cons (syntax (lit ...))
                                                (map
                                                 (lambda (rule)
                                                   (letrec* ((rf (syntax-form rule))
                                                             (pat (car rf))
                                                             (tmpl (cadr rf)))
                                                     (list (make-syntax
                                                            (cons head (cdr (syntax-form pat)))
                                                            def-ctx def-lib)
                                                           (datum->syntax def-stx
                                                             (list 'syntax tmpl)))))
                                                  rules)))))))))))))