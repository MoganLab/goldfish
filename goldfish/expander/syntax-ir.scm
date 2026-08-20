;;; syntax-ir.scm -- bridge: fully-expanded syntax -> IR record tree.
;;;
;;; The expander core resolves every identifier to a binding (lexical /
;;; primitive / toplevel / core-form / transformer) during expansion, but
;;; its lowered output (syntax objects) encodes that decision as a bare
;;; symbol (a gensym for lexical/toplevel, the primitive name for
;;; primitives).  The compiler's core->ir must therefore re-derive what an
;;; identifier refers to by name matching alone.
;;;
;;; syntax->ir re-walks a FULLY-EXPANDED syntax tree (the value expand-expr
;;; returns, before lower), re-resolving each identifier against the
;;; expansion context so the binding KIND is preserved in the IR:
;;;
;;;   - a primitive binding produces a <primitive-ref> node, so the
;;;     backend never has to guess "is this symbol a primitive?";
;;;   - lexical bindings keep their allocated gensym symbol (the backend
;;;     resolves frame slots by assq, as today);
;;;   - toplevel / unbound keep their emitted name.
;;;
;;; This is the first step toward the expander emitting the IR directly:
;;; it proves the binding-kind information is available on fully-expanded
;;; syntax and can flow into the IR without re-architecting the emitter.
;;;
;;; Note: this file is NOT part of the expander kernel artifact
;;; (kernel-combined.scm).  It is loaded after both the kernel and the
;;; compiler are available, and re-exports the IR constructors it needs.

(define-library (goldfish expander syntax-ir)
  (import (scheme base)
          (goldfish)
          (goldfish compiler ir))
  (export syntax->ir
    expand->ir)
  (begin

    ;; binding-kind : binding -> symbol/#f
    ;; Map a resolved binding to its kind, or #f if the identifier did not
    ;; resolve to a value binding.
    (define (binding-kind binding)
      (cond
        ((not binding) #f)
        ((primitive-binding? binding) 'primitive)
        ((lexical-binding? binding) 'lexical)
        ((toplevel-binding? binding) 'toplevel)
        ((core-form-binding? binding) 'core-form)
        ((transformer-binding? binding) 'transformer)
        (else 'other)))

    ;; resolve-name : syntax ctx -> (values name kind)
    ;; Resolve an identifier syntax to its emitted name and binding kind.
    (define (resolve-name id ctx)
      (let*-values (((name binding) (resolve-identifier id ctx)))
        (values name (binding-kind binding))))

    ;; ir-ident : syntax ctx -> ir
    ;; Convert an identifier reference: primitive -> <primitive-ref>,
    ;; anything else -> the emitted name symbol.
    (define (ir-ident id ctx)
      (let*-values (((name kind) (resolve-name id ctx)))
        (if (eq? kind 'primitive)
          (make-primitive-ref #f name)
          name)))

    ;; datum-of : syntax -> datum
    ;; The plain datum of a syntax node (used for structural fields such as
    ;; lambda formals and let binding names, which the IR keeps as data).
    (define (datum-of s)
      (if (syntax? s) (syntax->datum s) s))

    ;; syntax->ir : syntax ctx -> ir
    (define (syntax->ir stx ctx)
      (cond
        ((not (syntax? stx)) stx)
        (else
         (let ((form (syntax-form stx)))
           (cond
             ((not (pair? form))
              (if (symbol? form)
                (ir-ident stx ctx)
                form))
             (else
              (let ((head (car form))
                    (head-name (if (syntax? (car form)) (syntax-form (car form)) (car form))))
                (case head-name
                  ((quote) (make-const #f (datum-of (cadr form))))
                  ((quote-syntax) (make-const #f (datum-of (cadr form))))
                  ((define)
                   (if (symbol? (syntax-form (cadr form)))
                     (make-define #f (datum-of (cadr form))
                                  (syntax->ir (caddr form) ctx))
                     (let* ((df (syntax-form (cadr form)))
                            (dname (datum-of (car df)))
                            (dformals (map datum-of (cdr df))))
                       (make-define #f dname
                                    (make-lambda #f dformals
                                                 (map (lambda (b) (syntax->ir b ctx))
                                                      (cddr form)))))))
                  ((lambda)
                   (make-lambda #f (map datum-of (syntax-form (cadr form)))
                                (map (lambda (b) (syntax->ir b ctx)) (cddr form))))
                                    ((if)
                   (let ((else-stx (and (pair? (cdddr form)) (cadddr form))))
                     (make-if #f (syntax->ir (cadr form) ctx)
                              (syntax->ir (caddr form) ctx)
                              (if else-stx (syntax->ir else-stx ctx) #f))))
                  ((begin)
                   (make-begin #f (map (lambda (b) (syntax->ir b ctx)) (cdr form))))
                  ((let)
                   (if (symbol? (syntax-form (cadr form)))
                     (let* ((name (datum-of (cadr form)))
                            (bindings (syntax-form (caddr form)))
                            (body (cdddr form)))
                       (make-letrec 'letrec
                                    (list (list name
                                                (make-lambda #f (map (lambda (b) (datum-of (car b))) bindings)
                                                             (map (lambda (b) (syntax->ir b ctx)) body))))
                                    (list (make-call #f name
                                                     (map (lambda (b) (syntax->ir (cadr b) ctx))
                                                          bindings)))))
                     (let ((bindings (syntax-form (cadr form))))
                       (make-let #f
                                 (map (lambda (b) (list (datum-of (car b))
                                                        (syntax->ir (cadr b) ctx)))
                                      bindings)
                                 (map (lambda (b) (syntax->ir b ctx)) (cddr form))))))
                  ((letrec letrec*)
                   (let ((bindings (syntax-form (cadr form))))
                     (make-letrec head-name
                                  (map (lambda (b) (list (datum-of (car b))
                                                         (syntax->ir (cadr b) ctx)))
                                       bindings)
                                  (map (lambda (b) (syntax->ir b ctx)) (cddr form)))))
                  ((set!)
                   (make-set! #f (ir-ident (cadr form) ctx)
                              (syntax->ir (caddr form) ctx)))
                  ((values)
                   (make-values #f (map (lambda (b) (syntax->ir b ctx)) (cdr form))))
                  ((call-with-values)
                   (make-call-with-values #f (syntax->ir (cadr form) ctx)
                                          (syntax->ir (caddr form) ctx)))
                   (else
                    (make-call #f (syntax->ir head ctx)
                               (map (lambda (a) (syntax->ir a ctx)) (cdr form))))))))))))

    ;; expand->ir : datum -> ir
    ;; Expand a datum expression in the base library and convert it to IR.
    (define (expand->ir expr)
      (let*-values (((stx ctx) (expand-expr (wrap-expression expr) (initial-context))))
        (syntax->ir stx ctx)))))
