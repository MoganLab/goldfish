;;; expand.scm
;;; Main expression expander.
;;;
;;; (expand-expr stx ctx) -> (values sexp ctx)
;;;
;;; Output is a plain core Scheme S-expression.

(define (self-evaluating? x)
  (or (number? x) (boolean? x) (string? x) (char? x)))

(define (expand-expr stx ctx)
  (cond
    ((not (syntax? stx))
     (values (if (symbol? stx) (list 'quote stx) stx) ctx))
    ((not (pair? (syntax-form stx)))
     (expand-atom stx ctx))
    (else
     (expand-pair stx ctx))))

(define (expand-list stxs ctx)
  (if (null? stxs)
      (values '() ctx)
      (let*-values (((a ctx1) (expand-expr (car stxs) ctx))
                    ((as ctx2) (expand-list (cdr stxs) ctx1)))
        (values (cons a as) ctx2))))

(define (resolve-identifier stx ctx)
  (let ((name (context-resolve ctx stx)))
    (let ((binding (env-lookup (context-env ctx) name)))
      (if binding
          (values name binding)
          (let ((lib (syntax-library stx)))
            (let ((lib-binding (and lib (exp-library-ref lib name))))
              (if lib-binding
                  (values name lib-binding)
                  (let ((base-binding (let ((bl (base-library)))
                                        (and bl (exp-library-ref bl name)))))
                    (if base-binding
                        (values name base-binding)
                        (values name #f))))))))))

(define (expand-atom stx ctx)
  (let ((form (syntax-form stx)))
    (if (symbol? form)
        (let*-values (((name binding) (resolve-identifier stx ctx)))
          (cond
            ((core-form-binding? binding)
             (error "expand-atom: keyword used as expression" form))
            ((transformer-binding? binding)
             (error "expand-atom: macro used as expression" form))
            ((toplevel-binding? binding)
             (values (emit-toplevel-ref (binding-value binding) stx) ctx))
            (binding
             (values (make-syntax (binding-value binding)
                                  (syntax-context stx) (syntax-library stx))
                     ctx))
            (else
             (values (make-syntax name
                                  (syntax-context stx) (syntax-library stx))
                     ctx))))
        (values (if (self-evaluating? form)
                    (make-syntax form
                                 (syntax-context stx) (syntax-library stx))
                    (datum->syntax stx (list 'quote form)))
                ctx))))

;;; emit-toplevel-ref : toplevel-ref syntax -> syntax
;;; Reference to a module-defined toplevel: a bare gensym when the
;;; reference sits in the defining library (or the binding has no home),
;;; a qualified (module-ref 'home 'original) otherwise.

(define (emit-toplevel-ref ref src-stx)
  (let ((home (toplevel-ref-home ref)))
    (if (or (not home) (eq? home (syntax-library src-stx)))
        (make-syntax (toplevel-ref-gensym ref)
                     (syntax-context src-stx) (syntax-library src-stx))
        (datum->syntax src-stx
          (list 'module-ref
                (list 'quote (exp-library-name home))
                (list 'quote (toplevel-ref-original ref)))))))

;;; expand-macro-once : run a transformer once and return its (flipped)
;;; output WITHOUT expanding it.  The body scan uses this to see the head
;;; of an expansion (to detect macro-generated definitions) without
;;; recursing into expression bodies -- recursing there re-expands
;;; continuation-passing macros (match.scm) exponentially.

(define (expand-macro-once stx ctx proc)
  (let*-values (((scp-u ctx1) (context-alloc-scope ctx))
                ((scp-i ctx2) (context-alloc-scope ctx1)))
    (let ((ph (context-phase ctx))
          (ctx3 (context-with-intro-scope
                 (context-add-use-scope
                  (context-add-prune-scope ctx2 scp-u)
                  scp-u)
                 scp-i)))
      (set-current-expand-context! ctx3)
      (let* ((input (stx-flip-scope (stx-add-scope stx scp-u ph) scp-i ph))
             (output (proc input)))
        (values (stx-flip-scope output scp-i ph)
                (current-expand-context))))))

(define (expand-macro stx ctx proc)
  (let*-values (((output ctx4) (expand-macro-once stx ctx proc)))
    (let*-values (((sexp ctx5) (expand-expr output ctx4)))
      (values sexp
              (context-with-use-scopes (context-return ctx ctx5)
                                       (context-use-scopes ctx5))))))

(define (make-syntax-introducer)
  (let ((ctx (current-expand-context)))
    (unless ctx
      (error "make-syntax-introducer: no expansion context"))
    (let*-values (((scp ctx1) (context-alloc-scope ctx)))
      (set-current-expand-context! ctx1)
      (let ((ph (context-phase ctx)))
        (lambda (stx)
          (stx-flip-scope stx scp ph))))))

;;; syntax-local-introduce : syntax -> syntax
;;; Flip the current macro-introduction scope (scp_i), mapping an
;;; introduced identifier to its use-site form and vice versa.  Outside a
;;; macro (no intro scope) it is the identity.

(define (syntax-local-introduce stx)
  (let ((ctx (current-expand-context)))
    (unless ctx
      (error "syntax-local-introduce: no expansion context"))
    (let* ((ph (context-phase ctx))
           (stx1 (stx-maybe-flip stx (context-intro-scope ctx) ph)))
      (let loop ((s stx1) (scps (context-use-scopes ctx)))
        (if (null? scps)
            s
            (loop (stx-flip-scope s (car scps) ph) (cdr scps)))))))

(define (syntax-local-value id)
  (let ((ctx (current-expand-context)))
    (unless ctx
      (error "syntax-local-value: no expansion context"))
    (let*-values (((name binding) (resolve-identifier id ctx)))
      binding)))

;;; local-binder : syntax -> syntax
;;; Model LOCAL-BINDER: prune the accumulated use-site scopes (scps_u)
;;; off an identifier so it can serve as a binder in expanded output.

(define (local-binder id)
  (let ((ctx (current-expand-context)))
    (unless ctx
      (error "local-binder: no expansion context"))
    (stx-prune-scopes id (context-use-scopes ctx) (context-phase ctx))))

;;; local-expand : syntax [stops] -> syntax
;;; Model LOCAL-EXPAND (2-arg).  Build an environment where every binding
;;; is unstopped, then re-stop the given identifiers (wrapping their
;;; current bindings as TStop), flip the input by the intro scope, expand,
;;; and flip the result back.  Expansion always yields a syntax object
;;; (lowered later by `lower').

(define (local-expand stx . maybe-rest)
  (let ((ctx (current-expand-context)))
    (unless ctx
      (error "local-expand: no expansion context"))
    (let ((stops (if (null? maybe-rest) '() (car maybe-rest)))
          (maybe-defs (if (or (null? maybe-rest) (null? (cdr maybe-rest)))
                          #f
                          (cadr maybe-rest))))
      (let*-values (((result ctx1)
                     (if maybe-defs
                         (ctx-local-expand-defs ctx stx stops maybe-defs)
                         (ctx-local-expand ctx stx stops))))
        (set-current-expand-context! ctx1)
        result))))

;;; stx-flip-intro-off : syntax scope/#f phase -> syntax
;;; Remove the introduction scope if present (never add it): normalizes
;;; a binder or transformer RHS arriving from macro output (which carries
;;; the expand-macro output-flip scope) back to expansion space.  Literal
;;; body forms, already in expansion space, pass through unchanged.

(define (stx-flip-intro-off stx scp-i ph)
  (if (and scp-i (memq scp-i (syntax-scopes stx ph)))
      (stx-flip-scope stx scp-i ph)
      stx))

;;; local-expand-body : syntax stops defs -> syntax
;;; Body-scan expansion (internal-definition contexts): definition-
;;; context local expansion WITHOUT the intro-scope flip (body forms are
;;; already in expansion space; see the intdef.scm header for why).
;;; Sets current-expand-context like local-expand.

(define (local-expand-body stx stops defs)
  (let ((ctx (current-expand-context)))
    (unless ctx
      (error "local-expand-body: no expansion context"))
    (let*-values (((result ctx1)
                   (ctx-local-expand-defs* ctx stx stops defs #f)))
      (set-current-expand-context! ctx1)
      result)))

;;; build-stop-frame : context stops env -> alist
;;; A stop frame: each stop identifier resolves to its current binding in
;;; env, wrapped as a TStop.

(define (build-stop-frame ctx stops env)
  (map (lambda (s)
         (let ((resolved (if (identifier? s) (context-resolve ctx s) s)))
           (cons resolved (make-tstop-binding (env-lookup env resolved)))))
       stops))

(define (ctx-local-expand ctx stx stops)
  (let* ((ph (context-phase ctx))
         (scp-i (context-intro-scope ctx))
         (base-env (context-env ctx))
         (env-unstops (env-map-values binding-unstop base-env))
         (stop-frame (build-stop-frame ctx stops base-env))
         (env-stops (cons stop-frame env-unstops))
         (stx1 (stx-maybe-flip stx scp-i ph)))
    (let*-values (((sexp ctx2) (expand-expr stx1 (context-with-env ctx env-stops))))
      (values (stx-maybe-flip sexp scp-i ph)
              (context-with-use-scopes (context-return ctx ctx2)
                                       (context-use-scopes ctx2))))))

(define (ctx-local-expand-defs ctx stx stops defs)
  (ctx-local-expand-defs* ctx stx stops defs #t))

;;; ctx-local-expand-defs* : definition-context local expansion.  flip?
;;; is false for body-scan expansion (local-expand-body); see the
;;; intdef.scm header for why body forms expand without the intro flip.

(define (ctx-local-expand-defs* ctx stx stops defs flip?)
  (let* ((ph (context-phase ctx))
         (scp-i (context-intro-scope ctx))
         (scp-in (defs-scp-in defs))
         (addr-env (defs-addr defs))
         (env-defs (store-def-env-ref (context-store ctx) addr-env))
         (env-unstops (env-map-values binding-unstop env-defs))
         (stop-frame (build-stop-frame ctx stops env-defs))
         (env-stops (cons stop-frame env-unstops))
         (stx1 (if flip? (stx-maybe-flip stx scp-i ph) stx))
         (stx2 (stx-add-scope stx1 scp-in ph)))
    (let*-values (((sexp ctx2) (expand-expr stx2 (context-with-env ctx env-stops))))
      (values (if flip? (stx-maybe-flip sexp scp-i ph) sexp)
              (context-with-use-scopes (context-return ctx ctx2)
                                       (context-use-scopes ctx2))))))

;;; lower : syntax -> sexp
;;; Lower a fully-expanded syntax object to an evaluable core Scheme
;;; S-expression: strip contexts from the code spine.
;;;   (quote <x>)        -> (quote <datum>)    datum contents (handlers
;;;                        may have wrapped the datum while building
;;;                        output; syntax->datum normalizes it back)
;;;   (quote-syntax <x>) -> (quote <syntax>)   syntax literal: s7's eval
;;;                        of a quoted inlet yields the syntax object
;;;                        itself (expansion of `syntax'; instantiate in
;;;                        transformer code consumes it)

;;; lower-head : the leading symbol of a pair form, whether the head is a
;;; syntax identifier or a raw symbol (expand-expr's non-syntax fallback
;;; emits raw `(quote <sym>)').
(define (lower-head form)
  (let ((h (car form)))
    (if (syntax? h) (syntax-form h) h)))

(define (lower stx)
  (if (not (syntax? stx))
      stx
      (let ((form (syntax-form stx)))
        (cond
          ((pair? form)
           (cond
             ((and (pair? (cdr form)) (eq? (lower-head form) 'quote))
              (list 'quote (syntax->datum (cadr form))))
             ((and (pair? (cdr form)) (eq? (lower-head form) 'quote-syntax))
              (list 'quote (cadr form)))
             (else
              (map-spine lower form))))
          (else form)))))

(define-record-type <defs>
  (make-defs scp-in addr)
  defs?
  (scp-in defs-scp-in)
  (addr defs-addr))

(define (defs-scope d) (defs-scp-in d))

(define (new-defs)
  (let ((ctx (current-expand-context)))
    (unless ctx
      (error "new-defs: no expansion context"))
    (let*-values (((scp-in ctx1) (context-alloc-scope ctx))
                  ((addr-env ctx2) (context-alloc-def-env ctx1)))
      (let* ((store (store-def-env-set (context-store ctx2) addr-env (context-env ctx)))
             (ctx3 (context-add-prune-scope (context-with-store ctx2 store) scp-in)))
        (set-current-expand-context! ctx3)
        (make-defs scp-in addr-env)))))

(define (def-bind! defs id . maybe-transformer-stx)
  (let ((ctx (current-expand-context)))
    (unless ctx
      (error "def-bind!: no expansion context"))
    (let* ((scp-in (defs-scp-in defs))
           (addr-env (defs-addr defs))
           (ph (context-phase ctx))
           (scp-i (context-intro-scope ctx))
           (id1 (stx-flip-intro-off id scp-i ph))
            (id2 (stx-prune-scopes id1 (context-use-scopes ctx) ph))
            (id-defs (stx-add-scope id2 scp-in ph)))
      (if (null? maybe-transformer-stx)
          (let*-values (((name ctx1) (context-alloc-name ctx id-defs)))
            (let* ((ctx2 (context-bind ctx1 id-defs name))
                   (store (context-store ctx2))
                   (env-defs (store-def-env-ref store addr-env))
                   (env-new (env-extend env-defs name (make-lexical-binding name)))
                   (ctx3 (context-with-store ctx2 (store-def-env-set store addr-env env-new)))
                   (ctx4 (context-extend-env ctx3 name (make-lexical-binding name))))
              (set-current-expand-context! ctx4)
              name))
          (let* ((transformer-stx (car maybe-transformer-stx))
                 (stx1 (stx-flip-intro-off transformer-stx scp-i ph))
                 (stx2 (stx-add-scope stx1 scp-in ph)))
            (let*-values (((proc ctx1) (eval-transformer stx2 ctx)))
              (let*-values (((name ctx2) (context-alloc-name ctx1 id-defs)))
                (let* ((ctx3 (context-bind ctx2 id-defs name))
                       (store (context-store ctx3))
                       (env-defs (store-def-env-ref store addr-env))
                       (env-new (env-extend env-defs name (make-transformer-binding proc)))
                       (ctx4 (context-with-store ctx3 (store-def-env-set store addr-env env-new)))
                       (ctx5 (context-extend-env ctx4 name (make-transformer-binding proc))))
                  (set-current-expand-context! ctx5)
                  (if #f #f)))))))))

(define (expand-box val)
  (let ((ctx (current-expand-context)))
    (unless ctx
      (error "expand-box: no expansion context"))
    (let*-values (((addr ctx1) (context-alloc-box ctx)))
      (let ((ctx2 (context-with-store ctx1 (store-box-set (context-store ctx1) addr val))))
        (set-current-expand-context! ctx2)
        addr))))

(define (expand-unbox addr)
  (let ((ctx (current-expand-context)))
    (unless ctx
      (error "expand-unbox: no expansion context"))
    (store-box-ref (context-store ctx) addr)))

(define (expand-set-box! addr val)
  (let ((ctx (current-expand-context)))
    (unless ctx
      (error "expand-set-box!: no expansion context"))
    (set-current-expand-context!
     (context-with-store ctx (store-box-set (context-store ctx) addr val)))
    val))

(define (expand-pair stx ctx)
  (let ((form (syntax-form stx)))
    (let ((head (car form)))
      (if (identifier? head)
          (let*-values (((name binding) (resolve-identifier head ctx)))
            (cond
              ((tstop-binding? binding)
               (values stx ctx))
              ((core-form-binding? binding)
               ((binding-value binding) stx ctx))
              ((transformer-binding? binding)
               (expand-macro stx ctx (binding-value binding)))
              (else
               (expand-application stx ctx))))
          (expand-application stx ctx)))))

(define (expand-application stx ctx)
  (let ((form (syntax-form stx))
        (ctx0 (context-reset-use-scopes ctx)))
    (let*-values (((fun ctx1) (expand-expr (car form) ctx0))
                  ((args ctx2) (expand-list (cdr form) ctx1)))
      (values (make-syntax (cons fun args)
                           (syntax-context stx) (syntax-library stx))
              ctx2))))

(module-define! the-expander-library 'expand-expr expand-expr)
(module-define! the-expander-library 'expand-macro-once expand-macro-once)
(module-define! the-expander-library 'expand-list expand-list)
(module-define! the-expander-library 'lower lower)
(module-define! the-expander-library 'resolve-identifier resolve-identifier)
(module-define! the-expander-library 'local-expand local-expand)
(module-define! the-expander-library 'local-expand-body local-expand-body)
(module-define! the-expander-library 'make-syntax-introducer make-syntax-introducer)
(module-define! the-expander-library 'syntax-local-introduce syntax-local-introduce)
(module-define! the-expander-library 'syntax-local-value syntax-local-value)
(module-define! the-expander-library 'local-binder local-binder)
(module-define! the-expander-library 'make-defs make-defs)
(module-define! the-expander-library 'defs? defs?)
(module-define! the-expander-library 'defs-scp-in defs-scp-in)
(module-define! the-expander-library 'defs-scope defs-scope)
(module-define! the-expander-library 'defs-addr defs-addr)
(module-define! the-expander-library 'new-defs new-defs)
(module-define! the-expander-library 'def-bind! def-bind!)
(module-define! the-expander-library 'expand-box expand-box)
(module-define! the-expander-library 'expand-unbox expand-unbox)
(module-define! the-expander-library 'expand-set-box! expand-set-box!)
