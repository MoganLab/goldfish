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
                                             (if (stx-vector? form)
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
               (lambda (template patvars outer-patvars src-stx var-ctx)
                 (letrec* ((lib (syntax-library src-stx)))
                   (datum->syntax src-stx
                     (list 'fast-instantiate
                           (list 'quote
                                 (parse-template (datum->syntax src-stx template)
                                                 (append patvars outer-patvars)))
                           (cons 'list
                                 (append
                                   (map (lambda (p)
                                          (list 'cons
                                                (list 'quote p)
                                                (make-syntax p var-ctx lib)))
                                        patvars)
                                   (map (lambda (p)
                                          (list 'cons (list 'quote p) p))
                                         outer-patvars))))))))

              (generic-recurse
               (lambda (form stx patvars outer-patvars sctx lib)
                 (make-syntax (spine-map (lambda (sub)
                                           (transform-syntax-body sub patvars outer-patvars sctx lib))
                                         form)
                              (syntax-context stx) (syntax-library stx))))

              (expand-with-syntax
               (lambda (stx form patvars outer-patvars sctx lib)
                 (define (elems stx)
                   (let ((e (syntax-e stx)))
                     (if (pair? e)
                         (cons (car e) (elems (cdr e)))
                         '())))
                  (letrec* ((all (elems stx))
                            (bindings-elems (if (pair? all) (elems (cadr all)) '()))
                            (bindings-pairs (map elems bindings-elems))
                            (inner-pats (map (lambda (b) (if (pair? b) (car b) #f))
                                             bindings-pairs))
                            (inner-exprs (map (lambda (b) (if (and (pair? b) (pair? (cdr b)))
                                                               (cadr b)
                                                               #f))
                                              bindings-pairs))
                            (inner-patvars (apply append
                                                  (map (lambda (p)
                                                         (if (syntax? p)
                                                             (pattern-variables p '())
                                                             '()))
                                                       inner-pats)))
                           (xformed-exprs
                            (map (lambda (e)
                                   (transform-syntax-body e patvars outer-patvars sctx lib))
                                 inner-exprs))
                           (body-elems (if (pair? all) (cddr all) '()))
                           (body-stx (make-syntax (cons (make-syntax 'begin sctx lib)
                                                        body-elems)
                                                  sctx lib))
                           (body-xformed
                            (transform-syntax-body body-stx inner-patvars patvars sctx lib))
                           (ws-name (make-fresh-name 'ws-input)))
                   (datum->syntax stx
                     (list 'let
                           (list (list ws-name (cons 'list xformed-exprs)))
                           (list 'syntax-case-dispatch
                                 ws-name
                                 (list (make-syntax 'syntax sctx lib)
                                       (make-syntax '() sctx lib))
                                 (list 'list
                                       (list 'list
                                             (list 'syntax (make-syntax inner-pats sctx lib))
                                             (list 'quote inner-patvars)
                                             (list 'lambda inner-patvars '#t)
                                             (list 'lambda inner-patvars body-xformed)))))))))

              (transform-syntax-body
               (lambda (stx patvars outer-patvars sctx lib)
                 (letrec* ((form (syntax-form stx)))
                   (if (pair? form)
                       (if (identifier? (car form))
                           (letrec* ((head (syntax-form (car form))))
                             (if (eq? head 'syntax)
                                 (build-instantiate-call (cadr form) patvars outer-patvars stx sctx)
                                 (if (eq? head 'syntax-case)
                                     (make-syntax
                                      (cons (car form)
                                            (cons (transform-syntax-body (cadr form) patvars outer-patvars sctx lib)
                                                  (cons (caddr form) (cdddr form))))
                                      (syntax-context stx) (syntax-library stx))
                                     (if (eq? head 'with-syntax)
                                         (expand-with-syntax stx form patvars outer-patvars sctx lib)
                                         (generic-recurse form stx patvars outer-patvars sctx lib)))))
                           (generic-recurse form stx patvars outer-patvars sctx lib))
                       stx))))

               (compile-clause
                (lambda (clause-stx literal-ids sctx lib)
                  (letrec* ((clause (syntax-form clause-stx))
                            (pattern-stx (car clause))
                            (rest (cdr clause))
                            (fender-stx (if (= 2 (length rest)) (car rest) #f))
                            (body-stx (if (= 2 (length rest)) (cadr rest) (car rest)))
                            (patvars (pattern-variables pattern-stx literal-ids)))
                    (if (not (or (= 1 (length rest)) (= 2 (length rest))))
                        (error "syntax-case: expected a pattern, an optional guard expression, and an expression"
                               clause-stx)
                        (list 'list
                              (list 'syntax pattern-stx)
                              (list 'quote patvars)
                              (list 'lambda patvars
                                    (if fender-stx
                                        (transform-syntax-body fender-stx patvars '() sctx lib)
                                        #t))
                              (list 'lambda patvars
                                    (transform-syntax-body body-stx patvars '() sctx lib))))))))

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
;;; Also R7RS named ellipsis: (syntax-rules <ellipsis> (lit ...) (pat tmpl)
;;; ...) uses <ellipsis> as the ellipsis marker in patterns/templates (Guile
;;; behaves the same way).  A plain macro over syntax-case, mirroring Racket
;;; (syntax-rules is itself derived from syntax-case).  Each (pat tmpl)
;;; clause becomes a syntax-case clause (pat (syntax tmpl)); the generated
;;; transformer is (lambda (tmp) (syntax-case tmp (lit ...) ...)).
;;; Previously this desugaring lived in the kernel seam (transformer.scm);
;;; moving it here leaves eval-transformer as the only eval seam.
;;;
;;; Named ellipsis is implemented by substitution: the custom marker is
;;; replaced by `...' throughout every rule's pattern and template before
;;; the transformer is built, so the pattern-matching runtime keeps
;;; comparing against `...' (as R7RS requires, the custom marker then IS
;;; the ellipsis).  Known limitation (documented in the tests): a literal
;;; `...' that the user also writes inside a named-ellipsis rule is not
;;; escaped, so it too would be read as the ellipsis -- Guile escapes it.

(define (subst-ellipsis x e)
  ;; Replace every identifier whose form is e with `...', recursing through
  ;; syntax objects, datums, and embedded syntax values (syntax template
  ;; instantiation nests syntax objects inside datums).
  (cond
    ((syntax? x)
     (let ((form (syntax-form x)))
       (cond
         ((symbol? form)
          (if (eq? form e)
            (make-syntax '... (syntax-context x) (syntax-library x))
            x))
         ((pair? form)
          (make-syntax (cons (subst-ellipsis (car form) e)
                             (subst-ellipsis (cdr form) e))
                       (syntax-context x) (syntax-library x)))
         ((stx-vector? form)
          (make-syntax (list->vector
                        (map (lambda (d) (subst-ellipsis (datum->syntax x d) e))
                             (vector->list form)))
                       (syntax-context x) (syntax-library x)))
         (else x))))
    ((pair? x)
     (cons (subst-ellipsis (car x) e) (subst-ellipsis (cdr x) e)))
    ((vector? x)
     (vector-map (lambda (d) (subst-ellipsis d e)) x))
    ((eq? x e) '...)
    (else x)))

(define (sr-build-transformer def-stx tmp head lit rules)
  ;; Build the (lambda (tmp) (syntax-case tmp lit clause...)) datum for the
  ;; rules (each ((keyword . pattern) template)), replacing the rule head
  ;; with the fresh pattern variable `head'.
  (datum->syntax def-stx
    (cons 'lambda
          (cons (list tmp)
                (list (cons 'syntax-case
                            (cons tmp
                                  (cons lit
                                        (map (lambda (rule)
                                               (letrec* ((rf (syntax-form rule))
                                                         (pat (car rf))
                                                         (tmpl (cadr rf)))
                                                 (list (make-syntax
                                                        (cons head (cdr (syntax-form pat)))
                                                        (syntax-context def-stx)
                                                        (syntax-library def-stx))
                                                       (datum->syntax def-stx
                                                         (list 'syntax tmpl)))))
                                             rules)))))))))

(define-syntax syntax-rules
  (lambda (macro-stx)
    (syntax-case macro-stx ()
      ;; Default ellipsis: (syntax-rules (lit ...) ...).
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
      ((syntax-rules (lit ...) ((keyword . pattern) template) ...)
       (letrec* ((def-stx (syntax (list)))
                 (def-ctx (syntax-context def-stx))
                 (def-lib (syntax-library def-stx))
                 (tmp (make-syntax (make-fresh-name 'tmp) def-ctx def-lib))
                 (head (make-syntax (make-fresh-name 'kw) def-ctx def-lib)))
         (sr-build-transformer
           def-stx tmp head
           (syntax (lit ...))
           (syntax-form (syntax (((keyword . pattern) template) ...))))))
      ;; R7RS named ellipsis: (syntax-rules <ellipsis> (lit ...) ...).
      ;; The marker is substituted for `...' in every rule before the
      ;; transformer is built (see subst-ellipsis).
      ((syntax-rules ellipsis (lit ...) ((keyword . pattern) template) ...)
       (identifier? #'ellipsis)
       (letrec* ((def-stx (syntax (list)))
                 (def-ctx (syntax-context def-stx))
                 (def-lib (syntax-library def-stx))
                 (e (syntax-form #'ellipsis))
                 (tmp (make-syntax (make-fresh-name 'tmp) def-ctx def-lib))
                 (head (make-syntax (make-fresh-name 'kw) def-ctx def-lib)))
         (sr-build-transformer
           def-stx tmp head
           (syntax (lit ...))
           (map (lambda (r) (subst-ellipsis r e))
                (syntax-form (syntax (((keyword . pattern) template) ...))))))))))