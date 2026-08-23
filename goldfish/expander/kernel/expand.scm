;;; expand.scm
;;; Main expression expander.
;;;
;;; (expand-expr stx ctx) -> (values sexp ctx)
;;;
;;; Output is a plain core Scheme S-expression.

(define (self-evaluating? x)
  (or (number? x) (boolean? x) (string? x) (char? x)
      (bytevector? x)))

;;; keyword-symbol? : any -> boolean
;;; s7 keywords read as `:name' symbols (the R7RS reader has no keyword
;;; type), and define* also accepts the s7-style SUFFIX keyword `name:';
;;; the host evaluator treats a bare `:name' / `name:' reference as a
;;; self-evaluating keyword, so the expander must not resolve it as an
;;; identifier.

(define (keyword-symbol? x)
  (and (symbol? x)
       (let ((s (symbol->string x)))
         (and (> (string-length s) 0)
              (let ((first (string-ref s 0))
                    (last (string-ref s (- (string-length s) 1))))
                (or (eq? first #\:)
                    (eq? last #\:)))))))

(define-public (expand-expr stx ctx)
  (cond
    ((not (syntax? stx))
     (values (if (symbol? stx) (list 'quote stx) stx) ctx))
    ((not (pair? (syntax-form stx)))
     (expand-atom stx ctx))
    (else
     (expand-pair stx ctx))))

(define-public (expand-list stxs ctx)
  (if (null? stxs)
      (values '() ctx)
      (let*-values (((a ctx1) (expand-expr (car stxs) ctx))
                    ((as ctx2) (expand-list (cdr stxs) ctx1)))
        (values (cons a as) ctx2))))

;;; program-library? : exp-library/#f -> boolean
;;; True for a top-level program library (R7RS 5.1: a program's initial
;;; environment is empty -- its bindings come only from its imports, and
;;; free identifiers that resolve nowhere are errors, not ambient host
;;; names).  Library bodies (define-library) are NOT program libraries:
;;; their free names may still fall back to the base library / rootlet.

(define (program-library? lib)
  (and lib
       (let ((n (exp-library-name lib)))
         (and (pair? n) (eq? (car n) 'program)))))

(define-public (resolve-identifier stx ctx)
  (let ((name (context-resolve ctx stx)))
    (let ((binding (env-lookup (context-env ctx) name)))
      (if binding
          (values name binding)
          (let ((lib (syntax-library stx)))
            (let ((lib-binding (and lib (exp-library-ref lib name))))
              (if lib-binding
                  (values name lib-binding)
                  (if (program-library? lib)
                      (values name #f)
                      (let ((base-binding (let ((bl (base-library)))
                                            (and bl (exp-library-ref bl name)))))
                        (if base-binding
                            (values name base-binding)
                            (values name #f)))))))))))

(define (expand-atom stx ctx)
  (let ((form (syntax-form stx)))
    (if (symbol? form)
        (if (keyword-symbol? form)
            (values form ctx)
            (let*-values (((name binding) (resolve-identifier stx ctx)))
              (cond
                ((core-form-binding? binding)
                 (error "expand-atom: keyword used as expression" form))
                ((transformer-binding? binding)
                 (error "expand-atom: macro used as expression" form))
                ((toplevel-binding? binding)
                 (values (emit-toplevel-ref (binding-value binding) stx) ctx))
                (binding
                 ;; Only lexical and primitive bindings have a pure (symbol)
                 ;; value to inline.  Anything else is a live object (a
                 ;; module-form handler, a core/transformer procedure caught
                 ;; here, a stop wrapper) and must never be placed in a
                 ;; datum -- that would make the expanded output
                 ;; unserializable (cf. Racket, where datums are pure).  A
                 ;; stopped identifier stays unexpanded, exactly as
                 ;; expand-pair handles stops.
                 (cond
                   ((or (lexical-binding? binding) (primitive-binding? binding))
                    (values (make-syntax (binding-value binding)
                                         (syntax-context stx)
                                         (syntax-library stx))
                            ctx))
                   ((tstop-binding? binding)
                    (values stx ctx))
                   (else
                    (error "expand-atom: cannot inline live binding value"
                           form binding))))
                (else
                 (if (program-library? (syntax-library stx))
                     ;; Same error tag as the host evaluator's unbound
                     ;; reference (s7 signals 'unbound-variable at eval
                     ;; time), so (catch 'unbound-variable ...) / check-catch
                     ;; keep working -- the strict program environment just
                     ;; catches the reference earlier, at expansion time.
                     (error 'unbound-variable
                            "unbound identifier in program" form)
                     (values (make-syntax name
                                          (syntax-context stx)
                                          (syntax-library stx))
                             ctx))))))
        (values (if (self-evaluating? form)
                    (make-syntax form
                                 (syntax-context stx) (syntax-library stx))
                    (datum->syntax stx (list 'quote form)))
                ctx))))

;;; emit-toplevel-ref : toplevel-ref syntax -> syntax
;;; Reference to a module-defined toplevel: a bare gensym when the
;;; reference sits in the defining library (or the binding has no home),
;;; a qualified (module-ref 'home 'original) otherwise.  Bindings whose
;;; home is the BASE library are ambient: they live in the expander's own
;;; module / the host rootlet under their ORIGINAL name (the install
;;; loader evaluates lib-layer defines into the-expander-library under the
;;; renamed gensym AND module-define! registers the original), so any
;;; cross-library datum reference (e.g. define-macro transformer output
;;; referencing install-defmacro-transformer) emits the bare original
;;; name instead of a (module-ref ...) that would need a runtime module
;;; the base library does not register.

(define (emit-toplevel-ref ref src-stx)
  (let ((home (toplevel-ref-home ref)))
    (cond
      ((not home)
       (make-syntax (toplevel-ref-gensym ref)
                    (syntax-context src-stx) (syntax-library src-stx)))
      ((eq? home (syntax-library src-stx))
       (make-syntax (toplevel-ref-gensym ref)
                    (syntax-context src-stx) (syntax-library src-stx)))
      ((eq? home (base-library))
       (make-syntax (toplevel-ref-gensym ref)
                    (syntax-context src-stx) (syntax-library src-stx)))
      ;; A program-library binding has no runtime module (its defs evaluate
      ;; into the-expander-library under the gensym), so a reference from a
      ;; macro's own library context must still emit the bare gensym --
      ;; (module-ref '(program) ...) has nothing to look up, and set! on it
      ;; needs the gensym too.
      ((program-library? home)
       (make-syntax (toplevel-ref-gensym ref)
                    (syntax-context src-stx) (syntax-library src-stx)))
      ;; A binding NOT exported from its home library (e.g. an internal
      ;; helper referenced by one of the library's own macro templates,
      ;; which R7RS resolves in the defining library's scope) has no
      ;; runtime module entry -- the runtime module inlet holds only
      ;; exports, and module-ref rejects the rest.  Emit the bare gensym:
      ;; the library's defs define it in the rootlet, where the evaluating
      ;; program (the-expander-library) sees it.
      ((not (toplevel-ref-exported? ref))
       (make-syntax (toplevel-ref-gensym ref)
                    (syntax-context src-stx) (syntax-library src-stx)))
      (else
       (datum->syntax src-stx
         (list 'module-ref
               (list 'quote (exp-library-name home))
               (list 'quote (toplevel-ref-original ref))))))))

;;; expand-macro-once : run a transformer once and return its (flipped)
;;; output WITHOUT expanding it.  The body scan uses this to see the head
;;; of an expansion (to detect macro-generated definitions) without
;;; recursing into expression bodies -- recursing there re-expands
;;; continuation-passing macros (match.scm) exponentially.

(define-public (expand-macro-once stx ctx proc)
  (let*-values (((scp-u ctx1) (context-alloc-scope ctx))
                ((scp-i ctx2) (context-alloc-scope ctx1)))
    (let ((ph (context-phase ctx))
          (ctx3 (context-with-intro-scope
                 (context-add-use-scope
                  (context-add-prune-scope ctx2 scp-u)
                  scp-u)
                 scp-i)))
      (set-current-expand-context! ctx3)
      (let ((input (stx-add-scope-unchecked stx scp-u ph)))
        ;; Introduction-scope marking happens at output-node construction
        ;; time (datum->syntax, template instantiation), not by an
        ;; output-wide flip: introduced nodes pick up scp_i as they are
        ;; built, input nodes (already-existing syntax) do not.
        (let ((old-intro (current-intro-scope)))
          (set-current-intro-scope! scp-i)
          (let ((output (proc input)))
            (set-current-intro-scope! old-intro)
            (if (not (syntax? output))
                (error "syntax-case: macro output is not a syntax object"
                       output)
                (values output
                        (current-expand-context)))))))))

(define (expand-macro stx ctx proc)
  (let*-values (((output ctx4) (expand-macro-once stx ctx proc)))
    (let*-values (((sexp ctx5) (expand-expr output ctx4)))
      (values sexp
              (context-with-use-scopes (context-return ctx ctx5)
                                       (context-use-scopes ctx5))))))

(define-public (make-syntax-introducer)
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

(define-public (syntax-local-introduce stx)
  (let ((ctx (current-expand-context)))
    (unless ctx
      (error "syntax-local-introduce: no expansion context"))
    (let* ((ph (context-phase ctx))
           (stx1 (stx-maybe-flip stx (context-intro-scope ctx) ph)))
      (let loop ((s stx1) (scps (context-use-scopes ctx)))
        (if (null? scps)
            s
            (loop (stx-flip-scope s (car scps) ph) (cdr scps)))))))

(define-public (syntax-local-value id)
  (let ((ctx (current-expand-context)))
    (unless ctx
      (error "syntax-local-value: no expansion context"))
    (let*-values (((name binding) (resolve-identifier id ctx)))
      binding)))

;;; local-binder : syntax -> syntax
;;; Model LOCAL-BINDER: prune the accumulated use-site scopes (scps_u)
;;; off an identifier so it can serve as a binder in expanded output.

(define-public (local-binder id)
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

(define-public (local-expand stx . maybe-rest)
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

(define-public (local-expand-body stx stops defs)
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

(define-public (lower stx)
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

(define-record-type/public <defs>
  (make-defs scp-in addr)
  defs?
  (scp-in defs-scp-in)
  (addr defs-addr))

(define-public (defs-scope d) (defs-scp-in d))

(define-public (new-defs)
  (let ((ctx (current-expand-context)))
    (unless ctx
      (error "new-defs: no expansion context"))
    (let*-values (((scp-in ctx1) (context-alloc-scope ctx))
                  ((addr-env ctx2) (context-alloc-def-env ctx1)))
      (let* ((store (store-def-env-set (context-store ctx2) addr-env (context-env ctx)))
             (ctx3 (context-add-prune-scope (context-with-store ctx2 store) scp-in)))
        (set-current-expand-context! ctx3)
        (make-defs scp-in addr-env)))))

(define-public (def-bind! defs id . maybe-transformer-stx)
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
            (let*-values (((proc ctx1 _) (eval-transformer stx2 ctx)))
              (let*-values (((name ctx2) (context-alloc-name ctx1 id-defs)))
                (let* ((ctx3 (context-bind ctx2 id-defs name))
                       (store (context-store ctx3))
                       (env-defs (store-def-env-ref store addr-env))
                       (env-new (env-extend env-defs name (make-transformer-binding proc)))
                       (ctx4 (context-with-store ctx3 (store-def-env-set store addr-env env-new)))
                       (ctx5 (context-extend-env ctx4 name (make-transformer-binding proc))))
                  (set-current-expand-context! ctx5)
                  (if #f #f)))))))))

(define-public (expand-box val)
  (let ((ctx (current-expand-context)))
    (unless ctx
      (error "expand-box: no expansion context"))
    (let*-values (((addr ctx1) (context-alloc-box ctx)))
      (let ((ctx2 (context-with-store ctx1 (store-box-set (context-store ctx1) addr val))))
        (set-current-expand-context! ctx2)
        addr))))

(define-public (expand-unbox addr)
  (let ((ctx (current-expand-context)))
    (unless ctx
      (error "expand-unbox: no expansion context"))
    (store-box-ref (context-store ctx) addr)))

(define-public (expand-set-box! addr val)
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
              ((memq (syntax-form head) host-forms)
               ;; s7 host statement form (with-let etc.): pass through to the
               ;; host evaluator untouched.
               (values stx ctx))
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

;;; s7 host forms that must NOT be traversed by the expander: their bodies
;;; are s7 statements (definitions / environment forms) that the expander
;;; would reject in expression position (e.g. (with-let (unlet) (define ...
;;; ...)) in (liii case)).  They are passed through to the host evaluator,
;;; which understands them.  Identifiers are matched by name (these are
;;; ambient host forms, not user-shadowable bindings in the goldfish libs).

(define host-forms '(with-let sublet unlet))

