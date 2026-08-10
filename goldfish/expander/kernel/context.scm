;;; context.scm
;;; Expansion context.
;;;
;;; A context bundles:
;;;   phase        -- current phase number
;;;   env          -- environment mapping names to bindings
;;;   store        -- expand-time store
;;;   use-scopes   -- scps_u: use-site scopes accumulated during macro
;;;                   expansion at the current phase (pruned by local-binder)
;;;   prune-scopes -- scps_p: scopes to prune from generated syntax at current phase
;;;   defctx       -- current definition context (or #f)
;;;   intro-scope  -- scp_i: current macro-introduction scope (or #f = no-scope)

;;; Commentary
;;;
;;; Bindings classify identifiers as one of:
;;;   lexical    -- local variable, value is its gensym
;;;   toplevel   -- top-level variable, value is its source name
;;;   primitive  -- known primitive, value is its source name
;;;   transformer -- macro transformer procedure
;;;   core-form  -- built-in special-form handler
;;;   module-form -- top-level module-system form (define-library / import);
;;;                  handler (stx ctx) -> (values defs ctx), dispatched by the
;;;                  driver, not expanded as an expression

;;; Toplevel reference: the value of a toplevel binding.  Carries the
;;; allocated gensym plus module provenance: `home' is the exp-library
;;; the name was defined in (or #f for module-less toplevels), `original'
;;; the source name, `exported?' whether the defining library exports it.
;;; Cross-library references are emitted as (module-ref 'home 'original);
;;; same-library references as the bare gensym (see expand.scm).

(define-record-type <toplevel-ref>
  (make-toplevel-ref gensym home original exported?)
  toplevel-ref?
  (gensym toplevel-ref-gensym)
  (home toplevel-ref-home)
  (original toplevel-ref-original)
  (exported? toplevel-ref-exported? set-toplevel-ref-exported!))

(define-record-type <binding>
  (make-binding kind value)
  binding?
  (kind binding-kind)
  (value binding-value))

(define (make-lexical-binding name)
  (make-binding 'lexical name))

(define (make-toplevel-binding ref)
  (make-binding 'toplevel ref))

(define (make-primitive-binding name)
  (make-binding 'primitive name))

(define (make-transformer-binding proc)
  (make-binding 'transformer proc))

(define (make-core-form-binding proc)
  (make-binding 'core-form proc))

(define (make-module-form-binding proc)
  (make-binding 'module-form proc))

(define (lexical-binding? b)
  (and (binding? b) (eq? (binding-kind b) 'lexical)))

(define (toplevel-binding? b)
  (and (binding? b) (eq? (binding-kind b) 'toplevel)))

(define (primitive-binding? b)
  (and (binding? b) (eq? (binding-kind b) 'primitive)))

(define (transformer-binding? b)
  (and (binding? b) (eq? (binding-kind b) 'transformer)))

(define (core-form-binding? b)
  (and (binding? b) (eq? (binding-kind b) 'core-form)))

(define (module-form-binding? b)
  (and (binding? b) (eq? (binding-kind b) 'module-form)))

;;; Stop wrapper (model TStop): local-expand wraps a binding as a stop so
;;; that the expander halts at that identifier and returns the form
;;; unexpanded.  binding-unstop removes the wrapper (model unstop).

(define (make-tstop-binding wrapped)
  (make-binding 'stop wrapped))

(define (tstop-binding? b)
  (and (binding? b) (eq? (binding-kind b) 'stop)))

(define (binding-unstop b)
  (if (tstop-binding? b) (binding-value b) b))

;;; Dynamic context for macro transformers.

(define *current-expand-context* #f)

(define (current-expand-context)
  *current-expand-context*)

(define (set-current-expand-context! ctx)
  (set! *current-expand-context* ctx))

(define-record-type <context>
  (make-context phase env store use-scopes prune-scopes defctx intro-scope)
  context?
  (phase context-phase)
  (env context-env)
  (store context-store)
  (use-scopes context-use-scopes)
  (prune-scopes context-prune-scopes)
  (defctx context-defctx)
  (intro-scope context-intro-scope))

(define (context-empty)
  (make-context 0 (env-empty) (store-empty) '() '() #f #f))

(define (context-at-phase ctx phase)
  (make-context phase
                (context-env ctx)
                (context-store ctx)
                '()
                '()
                (context-defctx ctx)
                #f))

(define (context-with-env ctx env)
  (make-context (context-phase ctx)
                env
                (context-store ctx)
                (context-use-scopes ctx)
                (context-prune-scopes ctx)
                (context-defctx ctx)
                (context-intro-scope ctx)))

(define (context-with-store ctx store)
  (make-context (context-phase ctx)
                (context-env ctx)
                store
                (context-use-scopes ctx)
                (context-prune-scopes ctx)
                (context-defctx ctx)
                (context-intro-scope ctx)))

(define (context-with-use-scopes ctx use-scopes)
  (make-context (context-phase ctx)
                (context-env ctx)
                (context-store ctx)
                use-scopes
                (context-prune-scopes ctx)
                (context-defctx ctx)
                (context-intro-scope ctx)))

(define (context-reset-use-scopes ctx)
  (context-with-use-scopes ctx '()))

(define (context-with-intro-scope ctx scp)
  (make-context (context-phase ctx)
                (context-env ctx)
                (context-store ctx)
                (context-use-scopes ctx)
                (context-prune-scopes ctx)
                (context-defctx ctx)
                scp))

(define (context-extend-env ctx name value)
  (context-with-env ctx (env-extend (context-env ctx) name value)))

(define (context-add-use-scope ctx scp)
  (make-context (context-phase ctx)
                (context-env ctx)
                (context-store ctx)
                (set-add (context-use-scopes ctx) scp)
                (context-prune-scopes ctx)
                (context-defctx ctx)
                (context-intro-scope ctx)))

(define (context-add-prune-scope ctx scp)
  (make-context (context-phase ctx)
                (context-env ctx)
                (context-store ctx)
                (context-use-scopes ctx)
                (set-add (context-prune-scopes ctx) scp)
                (context-defctx ctx)
                (context-intro-scope ctx)))

(define (context-resolve ctx id)
  (store-resolve (context-store ctx) (context-phase ctx) id))

(define (context-bind ctx id name)
  (context-with-store ctx
                      (store-bind (context-store ctx)
                                  (context-phase ctx)
                                  id
                                  name)))

(define (context-alloc-name ctx id)
  (let-values (((name store)
                (store-alloc-name (context-store ctx) id)))
    (values name (context-with-store ctx store))))

(define (context-alloc-scope ctx)
  (let-values (((scp store)
                (store-alloc-scope (context-store ctx))))
    (values scp (context-with-store ctx store))))

(define (context-alloc-box ctx)
  (let-values (((addr store)
                (store-alloc-box (context-store ctx))))
    (values addr (context-with-store ctx store))))

(define (context-alloc-def-env ctx)
  (let-values (((addr store)
                (store-alloc-def-env (context-store ctx))))
    (values addr (context-with-store ctx store))))

;;; context-return : caller-ctx result-ctx -> ctx
;;; In the models, env and scps_p flow downward while the store threads
;;; through.  On returning from a subexpression, keep the resulting
;;; store but restore everything else from the caller.

(define (context-return caller-ctx result-ctx)
  (make-context (context-phase caller-ctx)
                (context-env caller-ctx)
                (context-store result-ctx)
                (context-use-scopes caller-ctx)
                (context-prune-scopes caller-ctx)
                (context-defctx caller-ctx)
                (context-intro-scope caller-ctx)))

;;; free-identifier=? : syntax syntax [context] -> bool

(define (free-identifier=? id1 id2 . maybe-ctx)
  (let ((ctx (if (null? maybe-ctx) (current-expand-context) (car maybe-ctx))))
    (unless ctx
      (error "free-identifier=?: no expansion context"))
    (eq? (context-resolve ctx id1) (context-resolve ctx id2))))

;;; Library exports

(module-define! the-expander-library 'make-context make-context)
(module-define! the-expander-library 'context? context?)
(module-define! the-expander-library 'context-empty context-empty)
(module-define! the-expander-library 'context-at-phase context-at-phase)
(module-define! the-expander-library 'context-phase context-phase)
(module-define! the-expander-library 'context-return context-return)
(module-define! the-expander-library 'context-use-scopes context-use-scopes)
(module-define! the-expander-library 'context-with-use-scopes context-with-use-scopes)
(module-define! the-expander-library 'context-reset-use-scopes context-reset-use-scopes)
(module-define! the-expander-library 'context-intro-scope context-intro-scope)
(module-define! the-expander-library 'context-with-intro-scope context-with-intro-scope)
(module-define! the-expander-library 'context-with-env context-with-env)
(module-define! the-expander-library 'context-extend-env context-extend-env)
(module-define! the-expander-library 'context-add-use-scope context-add-use-scope)
(module-define! the-expander-library 'context-add-prune-scope context-add-prune-scope)
(module-define! the-expander-library 'context-resolve context-resolve)
(module-define! the-expander-library 'context-bind context-bind)
(module-define! the-expander-library 'context-alloc-name context-alloc-name)
(module-define! the-expander-library 'context-alloc-scope context-alloc-scope)
(module-define! the-expander-library 'context-alloc-box context-alloc-box)
(module-define! the-expander-library 'context-alloc-def-env context-alloc-def-env)
(module-define! the-expander-library 'current-expand-context current-expand-context)
(module-define! the-expander-library 'set-current-expand-context! set-current-expand-context!)
(module-define! the-expander-library 'make-binding make-binding)
(module-define! the-expander-library 'binding? binding?)
(module-define! the-expander-library 'make-lexical-binding make-lexical-binding)
(module-define! the-expander-library 'make-toplevel-binding make-toplevel-binding)
(module-define! the-expander-library 'make-primitive-binding make-primitive-binding)
(module-define! the-expander-library 'make-transformer-binding make-transformer-binding)
(module-define! the-expander-library 'make-core-form-binding make-core-form-binding)
(module-define! the-expander-library 'make-module-form-binding make-module-form-binding)
(module-define! the-expander-library 'lexical-binding? lexical-binding?)
(module-define! the-expander-library 'toplevel-binding? toplevel-binding?)
(module-define! the-expander-library 'primitive-binding? primitive-binding?)
(module-define! the-expander-library 'transformer-binding? transformer-binding?)
(module-define! the-expander-library 'core-form-binding? core-form-binding?)
(module-define! the-expander-library 'module-form-binding? module-form-binding?)
(module-define! the-expander-library 'make-tstop-binding make-tstop-binding)
(module-define! the-expander-library 'tstop-binding? tstop-binding?)
(module-define! the-expander-library 'binding-unstop binding-unstop)
(module-define! the-expander-library 'make-toplevel-ref make-toplevel-ref)
(module-define! the-expander-library 'toplevel-ref? toplevel-ref?)
(module-define! the-expander-library 'toplevel-ref-gensym toplevel-ref-gensym)
(module-define! the-expander-library 'toplevel-ref-home toplevel-ref-home)
(module-define! the-expander-library 'toplevel-ref-original toplevel-ref-original)
(module-define! the-expander-library 'toplevel-ref-exported? toplevel-ref-exported?)
(module-define! the-expander-library 'set-toplevel-ref-exported! set-toplevel-ref-exported!)
(module-define! the-expander-library 'free-identifier=? free-identifier=?)
