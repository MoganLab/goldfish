;;; lib/module.scm
;;; R7RS library surface as SELF-HOSTED lib-layer source (installed by
;;; lib/install.scm into the-base-library, not pre-expanded into the core
;;; artifact).  It adapts the kernel's minimal module machinery:
;;;
;;;   * Minimal module API (kernel): a module is an exp-library plus a
;;;     registry entry (exp-library . export-names).  expand-library-body
;;;     (expander/kernel/libbody.scm) expands a module body, installing its
;;;     defines into the library.  The driver knows nothing about R7RS; it
;;;     only dispatches top-level forms whose head resolves to a
;;;     `module-form' binding (see expander/kernel/context.scm).
;;;
;;;   * Runtime modules (Guile-style, S2): define-library also emits a
;;;     registration expression (make-module/module-define!/register-module,
;;;     see liii/prelude.scm) so libraries have runtime identity; cross-
;;;     library references are emitted as (module-ref 'lib 'name).  References
;;;     within the defining library stay bare gensyms.  Exported bindings are
;;;     immutable (set! on them is an expansion error): the module inlet holds
;;;     value snapshots, not shared cells.  Re-exports forward via module-ref;
;;;     references resolve straight to the defining module.
;;;
;;;   * On-demand file loading: importing an unknown library loads
;;;     (foo bar) -> foo/bar.scm from *load-path* at expand time.
;;;
;;;   * R7RS adapter: parses the R7RS `define-library' / `import' surface
;;;     syntax (export/import/only/prefix/rename clauses) and calls the
;;;     minimal API.  Guile-style `define-module' / `use-modules' desugar
;;;     to the R7RS forms (explicit-body semantics).  All surface forms are
;;;     installed into the-base-library as module-form bindings.
;;;
;;; The kernel core keeps only: exp-library (expander/kernel/exp-library.scm),
;;; expand-library-body (expander/kernel/libbody.scm), the binding types, and
;;; the runtime module substrate (liii/prelude.scm).
;;;
;;; Split boundary (do not move code across it lightly): each lib file
;;; installs as one unit, so a file may only reference names bound by
;;; earlier files (same-file forward refs hoist, cross-file ones go
;;; unbound).  Only the registry prefix satisfied this (now
;;; module-registry.scm); loader/cache/import/expand are mutually
;;; recursive (capture needs expand-define-library, the loader needs
;;; import-spec + capture) and stay whole in this file.


;;; Registry and instance state live in lib/module-registry.scm
;;; (installed just before this file).

;;; On-demand file loading.  A library name (foo bar) maps to the file
;;; "foo/bar.scm" searched over *load-path* (via the loader's single
;;; load-find-module-file).  The file must contain define-library forms; it
;;; is compiled (registering the expand-time record, recursively loading
;;; imports) and evaluated (registering the runtime module).
;;;
;;; (File-name mapping, dependency fingerprints, the import-graph walk,
;;; the cache key/validity backend, and the module-ref collector all live
;;; in the unified cache backend in lib/install.scm; this file uses them
;;; through the shared expander namespace.)

(define (library-cache-deps recs self)
  (let loop ((ls (collect-cache-module-refs recs)) (acc '()))
    (if (null? ls)
      (reverse acc)
      (if (or (equal? (car ls) self) (member (car ls) acc))
        (loop (cdr ls) acc)
        (loop (cdr ls) (cons (car ls) acc))))))

;;; Macro-provider dependencies: a consumer's cached defs bake in the
;;; expansions of the macros it imported, but a pure syntax macro leaves no
;;; module-ref behind, so module-ref collection alone misses the provider --
;;; editing its source would never invalidate the consumer.  Every library
;;; the file imports is therefore a dependency too.  The whole transitive
;;; closure is fingerprinted (source stamps), so a change anywhere in the
;;; import graph invalidates every consumer whose baked output could have
;;; been affected.  (Import-set bottoming, clause collection, the source
;;; walk, and the BFS closure live in the install.scm backend.)

;;; (collect-import-clause-libs lives in the install.scm backend.)

;;; (lib-source-import-libs and transitive-lib-closure live in the
;;; install.scm backend.)

(define (cache-record-import-libs rec)
  (let ((imports (lib-cache-imports rec)))
    (let loop ((groups imports) (acc '()))
      (if (null? groups)
        acc
        (let group ((specs (car groups)) (a acc))
          (if (null? specs)
            (loop (cdr groups) a)
            (let ((n (import-set-lib-name (car specs))))
              (if (and (pair? n) (member n a))
                (group (cdr specs) a)
                (group (cdr specs) (if (pair? n) (cons n a) a))))))))))

(define (library-import-deps recs self)
  (let loop ((rs recs) (acc '()))
    (if (null? rs)
      acc
      (let ((ls (cache-record-import-libs (car rs))))
        (loop (cdr rs)
              (let add ((ns ls) (a acc))
                (if (null? ns)
                  a
                  (if (or (equal? (car ns) self) (member (car ns) a))
                    (add (cdr ns) a)
                    (add (cdr ns) (cons (car ns) a))))))))))

;;; library-all-deps : recs self -> (list name)
;;; Every library this file can be invalidated by, transitively: the
;;; module-ref targets plus every library the file imports, closed over each
;;; dependency's own imports (a pure-syntax macro provider leaves no
;;; module-ref, and a change deep in the graph can still alter what a macro
;;; here expands to).
(define (library-all-deps recs self)
  (transitive-lib-closure
    (append (library-cache-deps recs self)
            (library-import-deps recs self))))

;;; load-library! : name -> void
;;; Compile (registering the expand-time record, recursively loading imports)
;;; and evaluate (registering the runtime module) a define-library file.
;;; Circular loads are an error (stricter than R7RS, which tolerates some
;;; import cycles).  File lookup reuses the loader's load-find-module-file.
;;;
;;; Since 2026-08-15 load-library! also uses the library cache: a library
;;; file whose top-level forms are all define-library is compiled once,
;;; captured into the ccache as a library cache record (bindings + macro
;;; specs + lowered defs), and later loads rebuild the expand-time registry,
;;; replay the macro definitions, and eval the lowered defs.  The cache is a
;;; compiled artifact of the source, used only while the unified validity
;;; gate holds (source stamp plus the transitive dependency fingerprints;
;;; see cache-load-checked in lib/install.scm).  A file with any
;;; non-library top-level form falls back to the previous compile-program
;;; path.

(define *libraries-being-loaded* '())

;;; Library-cache capture helpers.
;;;
;;; A library cache record is
;;;   (name exports ((id . binding-desc) ...) ((id . lowered-macro) ...) defs)
;;; where defs are the lowered value/registration forms, binding-desc is the
;;; purifiable description of a value binding, and each lowered-macro is the
;;; cached transformer form (re-evaluated in the loading unit's expand env
;;; on a hit).  On cache hit the exp-library is rebuilt from the binding
;;; descriptions, the macros are replayed, and defs are evaluated.

;;; library-top-level? : datum -> bool
;;; Whether every top-level form in a loaded file is a define-library.

(define (library-top-level? form)
  (and (pair? form)
       (eq? (car form) 'define-library)))

;;; library-file-cacheable? : (list datum) -> bool
;;; A file is cacheable when it is not empty and every top-level form is a
;;; define-library (so the cached defs exactly reproduce the file).

(define (library-file-cacheable? forms)
  (and (pair? forms)
       (let loop ((fs forms))
         (cond
           ((null? fs) #t)
           ((library-top-level? (car fs)) (loop (cdr fs)))
           (else #f)))))

;;; Library cache keys use the unified backend (cache-file-for): the
;;; library's relative file name (e.g. "srfi/srfi-13.scm") mirrored
;;; under the versioned cache dir, suffixed by optimization level
;;; (caches store defs already optimized, so levels stay separate).

;;; extract-exports : syntax -> (list symbol)

(define (extract-exports form)
  (let ((form (syntax-form form)))
    (apply append
             (map (lambda (ef)
                   (map (lambda (e)
                          (let ((d (syntax->datum e)))
                            (if (export-rename-spec? d) (caddr d) d)))
                        (cdr (syntax-form ef))))
                 (filter (lambda (cl)
                          (and (pair? (syntax-form cl))
                               (identifier? (car (syntax-form cl)))
                               (eq? (syntax-form (car (syntax-form cl))) 'export)))
                        (cddr form))))))

;;; purify-binding : binding -> datum
;;; Strict capture variant over the shared install-binding-desc core:
;;; transformer/core-form/module-form bindings cannot be serialized (their
;;; value is a closure), so a macro binding is recorded as the symbol
;;; 'transformer and replayed from its source spec.  A library whose
;;; bindings contain a core-form/module-form value (e.g. an exported
;;; define-library handler) is not cacheable and is signalled here.

(define (purify-binding b)
  (or (install-binding-desc b)
      (error "purify-binding: library not cacheable (unsupported binding)"
             (binding-kind b))))

;;; depurify restores through install-depurify-binding with the strict
;;; flag (unregistered homes are #f: the registry is fully available on
;;; this path, unlike the boot installs).

;;; capture-library-cache : syntax exp-library context
;;;                    -> (values (list name exports imports bindings macros defs renames) context)
;;; Expand one define-library form (which registers the expand-time record
;;; and returns its lowered defs) and capture everything needed to rebuild
;;; it without re-expansion: the value bindings (purified), the macro specs
;;; (source datums), and the lowered defs.

(define (capture-library-cache stx lib ctx)
  (let* ((form (syntax-form stx))
         (name (syntax->datum (cadr form))))
    ;; Expand: registers the library and returns its defs.
    (let*-values (((defs ctx1) (expand-define-library stx ctx)))
      (let* ((rec (library-registry-ref name))
             (lib1 (and rec (lib-record-library rec)))
             (exports (extract-exports stx))
             ;; Export renames ride the record so warm restore re-installs
             ;; the aliases (including aliases of imported macros, whose
             ;; closures only exist via the restored re-imports).
             (parsed (call-with-values
                       (lambda () (parse-library-clauses (cddr form)))
                       list))
             (imports (reverse (caddr parsed)))
             (renames (cadr parsed))
             (bindings (map (lambda (e)
                              (cons (car e) (purify-binding (cdr e))))
                            ;; Only the cacheable binding kinds are stored:
                            ;; imported core-form / module-form bindings (an
                            ;; import of (goldfish) pulls every ambient
                            ;; binding) have no serializable description and
                            ;; are re-derived from the re-imported
                            ;; dependencies at restore time.
                            (filter (lambda (e)
                                      (memq (binding-kind (cdr e))
                                            '(toplevel primitive transformer)))
                                    (exp-library-bindings lib1))))
              ;; Macro definitions are cached as their LOWERED transformer
              ;; forms, the same mechanism the boot library installs use:
              ;; expand-library-body collected them as (name . lowered) while
              ;; this define-library body expanded, and warm start
              ;; re-evaluates them (cf. Racket's direct-eval).  This replaced
              ;; the old source-spec replay, which could not recognize every
              ;; macro-defining form.
              (macros (map (lambda (m)
                              (cons (car m) (serialize-cache-sexp (cdr m))))
                            (take-collected-macros)))
              (def-ir (cache-defs->ir defs ctx1)))
        (values (list name exports imports bindings macros def-ir renames) ctx1)))))

;;; syntax-ir-fn : -> procedure/#f
;;; Lazily resolve syntax->ir/sexp from (goldfish expander tree-il) (L4).
;;; #f when the library is unavailable or is itself being captured (loading
;;; it would trip the circular-dependency check); callers fall back to
;;; lower.  Shared by cache-defs->ir and optimize-on-load.

(define (syntax-ir-fn)
  (if (member '(goldfish expander tree-il) *libraries-being-loaded*)
    #f
    (catch
      #t
      (lambda ()
        (if (not (runtime-registered? '(goldfish expander tree-il)))
          (load-library! '(goldfish expander tree-il)))
        (module-ref (lookup-module '(goldfish expander tree-il))
                    'syntax->ir/sexp))
      (lambda (tag . info) #f))))

;;; cache-defs->ir : (list syntax) context -> (list ir|sexp)
;;; Cache the defs as record tree-il (via syntax->ir/sexp) so the
;;; optimized cache path keeps <primitive-ref> binding kinds through the
;;; passes (lowered sexp loses that distinction).  The IR serializes
;;; cleanly (records are plain vectors; gensym names are interned
;;; symbols).  Falls back to lowered sexp when the compiler is not
;;; loaded (bootstrap: compile-defs-cached could not ir->core it back) or
;;; when (goldfish expander tree-il) is itself being captured (recursion
;;; guard: loading it would trip the circular-dependency check).

(define (cache-defs->ir defs ctx)
  (if (not (runtime-registered? '(goldfish compiler)))
    (map lower defs)
    (let ((s2ir (syntax-ir-fn)))
      (if (procedure? s2ir)
        (catch
          #t
          (lambda () (map (lambda (d) (s2ir d ctx)) defs))
          (lambda (tag . info) (map lower defs)))
        (map lower defs)))))

;;; capture-file-cache : (list datum) -> (values (list lib-cache) context)
;;; Capture every define-library form in a file, in order, returning the
;;; per-library cache records.

(define (capture-file-cache forms)
  (let loop ((fs forms) (ctx (initial-context)) (acc '()))
    (if (null? fs)
      (values (reverse acc) ctx)
      (let* ((stx (stx-set-library (wrap-expression (car fs)) the-base-library)))
        (let*-values (((rec ctx1) (capture-library-cache stx the-base-library ctx)))
          (loop (cdr fs) ctx1 (cons rec acc)))))))

;;; lib-cache field accessors.  A cache record is
;;;   (name exports imports bindings macros defs [renames])
;;; renames (a list of (to . from)) is present on records captured with
;;; export-rename support; older records default to no aliases.

(define (lib-cache-name rec) (car rec))
(define (lib-cache-exports rec) (cadr rec))
(define (lib-cache-imports rec) (caddr rec))
(define (lib-cache-bindings rec) (cadddr rec))
(define (lib-cache-macros rec) (car (cddddr rec)))
(define (lib-cache-defs rec) (cadr (cddddr rec)))
(define (lib-cache-renames rec)
  (if (> (length rec) 6) (list-ref rec 6) '()))

;;; restore-library-cache : lib-cache -> exp-library
;;; Rebuild a library from its cache record: re-import its dependencies
;;; (copying bindings, including re-exported macros, from their registries),
;;; restore its own value bindings, replay its macro definitions, and
;;; re-register it.  Returns the rebuilt library (defs are evaluated by the
;;; caller).

(define (restore-library-cache rec . maybe-level)
  (let* ((level (registry-level-arg maybe-level))
         (name (lib-cache-name rec))
         (exports (lib-cache-exports rec))
         (imports (lib-cache-imports rec))
         (bindings (lib-cache-bindings rec))
         (macros (lib-cache-macros rec))
         (lib (make-exp-library name)))
    ;; 1. Re-import dependencies: copies bindings (including transformer
    ;;    bindings re-exported from other libraries) into this library.
    (import-into-library! lib imports)
    ;; 2. Restore this library's own value bindings (toplevel-ref homes that
    ;;    point at the library itself resolve to the rebuilt library).
    (for-each (lambda (e)
                (let ((d (install-depurify-binding (cdr e) lib #t)))
                  (when d (exp-library-define! lib (car e) d))))
              bindings)
    (library-registry-set! name (make-lib-record lib exports) level)
    ;; 3. Rebuild this library's own macro transformers from their cached
    ;;    lowered forms: re-evaluating each form in the current unit's
    ;;    expand env yields the transformer (exactly where a cold
    ;;    expansion would put it), which is registered exactly as
    ;;    expand-lib-define-syntax does.
    ;;    This replaced the source-spec replay + expand-library-body, and is
    ;;    the same mechanism the boot library installs use -- one cache path
    ;;    for standard and user libraries.
    (for-each (lambda (m)
                (let* ((mname (car m))
                       (data (deserialize-cache-sexp (cdr m)))
                       (proc (eval data (current-expand-env))))
                  (exp-library-define! lib mname (make-transformer-binding proc))))
              macros)
     ;; 4. Re-install export renames: `from' resolves from the re-imports
     ;;    (imported macros included) and the restored/ replayed own
     ;;    definitions, exactly as the cold path aliases them.
     (for-each (lambda (r)
                 (let ((binding (exp-library-ref lib (cdr r))))
                   (unless binding
                     (error "define-library: export has no binding"
                            (cdr r) name))
                   (exp-library-define! lib (car r) binding)))
               (lib-cache-renames rec))
     ;; 5. Exports with no restored body/import binding are an error (they
     ;;    would have failed at capture time too: expand-define-library
     ;;    requires every export to resolve from the body or an import).
     (for-each (lambda (export)
                 (unless (exp-library-ref lib export)
                   (error "define-library: export has no binding" export name)))
               exports)
     lib))

;;; Per-level instances (moved after the cache section: rebuild calls
;;; lib-cache-name and restore-library-cache above).

(define *perlevel-saved-records* '())

(define (perlevel-snapshot-records forms)
  (let loop ((fs forms) (acc '()))
    (if (null? fs)
      acc
      (let ((f (car fs)))
        (if (and (pair? f) (eq? (car f) 'define-library) (pair? (cdr f)))
          (let ((n (cadr f)))
            (loop (cdr fs) (cons (cons n (library-registry-ref n)) acc)))
          (loop (cdr fs) acc))))))

(define (perlevel-rebuild! recs level saved-records)
  (for-each
    (lambda (r)
      (let ((n (lib-cache-name r)))
        (set! *library-registry*
              (filter (lambda (e) (not (equal? (car e) n)))
                      *library-registry*))))
    recs)
  ;; Re-add pre-existing valid bare instances clobbered by capture
  ;; (capture registers bare as a side effect, overwriting them).
  (for-each
    (lambda (e)
      (when (and (cdr e) (runtime-registered? (car e)))
        (library-registry-set! (car e) (cdr e))))
    saved-records)
  (for-each (lambda (r) (restore-library-cache r level)) recs))

;;; (collect-cache-module-refs lives in the install.scm backend.)
;;; Levels follow the -O0/1/2 convention (Guile-style; Guile defaults to
;;; 2); GOLDFISH_OPT_LEVEL controls cache-level in the install backend.

;;; compile-defs-on-load : (list syntax) context -> (list sexp)
;;; Apply the (goldfish compiler) pipeline to a library's defs.  The
;;; compiler library is loaded lazily (it is a normal load-path library, not
;;; part of the expander core), so its import must not disturb the bootstrap
;;; of the library machinery itself.  A failure to load the compiler leaves
;;; the defs lowered-but-unoptimized (compilation is an optimization, never
;;; a correctness requirement).  The active pass set grows with the
;;; optimization level: level 1 runs constant-fold + simplify-if, level 2
;;; adds the inline (peval) pass.
;;;
;;; The defs are UN-LOWERED syntax objects (the expand-library-body
;;; output), so the pipeline runs via syntax->ir: primitive references
;;; stay <primitive-ref> nodes through the passes.  The cached-file path
;;; (compile-defs-cached) runs the same syntax->ir IR through the passes
;;; and lowers it for the shared cache.

;;; compiler-module : -> module/#f
;;; Lazy-load (goldfish compiler); #f when unavailable (bootstrap phase,
;;; a load failure).  Compilation is an optimization, never a correctness
;;; requirement, so every caller degrades gracefully on #f.

(define (compiler-module)
  (catch
    #t
    (lambda ()
      (if (not (runtime-registered? '(goldfish compiler)))
        (load-library! '(goldfish compiler)))
      (lookup-module '(goldfish compiler)))
    (lambda (tag . info) #f)))

;;; compiler-pass-list : module level -> (list pass)
;;; The active pass set: level 1 = constant-fold + simplify-if; level 2+
;;; adds the inliner (copy propagation + beta reduction).  Order: fold
;;; constants first (the inliner relies on folded literals propagating),
;;; then inline, then clean up the ifs the inliner's pruning leaves
;;; behind.  lower-let is deliberately absent (see compile-defs-cached).

(define (compiler-pass-list compiler level)
  (let ((constant-fold (module-ref compiler 'constant-fold))
        (simplify-if (module-ref compiler 'simplify-if)))
    (if (>= level 2)
      (list constant-fold (module-ref compiler 'inline) simplify-if)
      (list constant-fold simplify-if))))

(define (compile-defs-on-load defs ctx)
  (let ((level (cache-level)))
    (if (zero? level)
      (map lower defs)
      (let ((compiler (compiler-module)))
        (if (module? compiler)
          (let ((compile-syntax-defs
                 (catch
                   #t
                   (lambda ()
                     (if (not (runtime-registered? '(goldfish compiler syntax-ir)))
                       (load-library! '(goldfish compiler syntax-ir)))
                     (module-ref (lookup-module '(goldfish compiler syntax-ir))
                                 'compile-syntax-defs))
                   (lambda (tag . info) #f))))
            (if (and (procedure? compile-syntax-defs))
              ;; NOTE: lower-let is deliberately excluded (see
              ;; compile-defs-cached): it restarts slot numbering at 0,
              ;; misaddressing syntax->ir's real lexical (depth . index)
              ;; refs when the let sits inside a lambda with outer formals.
              (compile-syntax-defs defs ctx (compiler-pass-list compiler level))
              (map lower defs)))
          (map lower defs))))))

;;; compile-defs-cached : (list ir|sexp) -> (list sexp)
;;; Apply the compiler pipeline to a cache record's defs (the cached-file
;;; path: capture-library-cache stores record tree-il via syntax->ir/sexp).
;;; IR defs run through the passes and lower to sexp (what the s7 evaluator
;;; eats); a lowered fallback def (bootstrap capture, when the compiler was
;;; unavailable) is returned as-is -- the pipeline only ever sees the IR
;;; records, core->ir is gone.

(define (compile-defs-cached defs)
  (let ((level (cache-level)))
    (if (or (null? defs) (not (vector? (car defs))))
      ;; Lowered fallback defs (bootstrap capture with the compiler
      ;; unavailable / tree-il itself being captured) pass through as-is;
      ;; the pipeline only ever optimizes the IR records.
      defs
      (let ((compiler (compiler-module)))
        (if (module? compiler)
          (let ((run-passes (module-ref compiler 'run-passes))
                (ir->core (module-ref compiler 'ir->core)))
            ;; NOTE: lower-let is deliberately NOT in the pass set.  It
            ;; lowers a <let> into a lambda-case whose formals restart slot
            ;; numbering at 0, but syntax->ir's IR carries real lexical
            ;; (depth . index) addresses computed against the enclosing
            ;; frame (env-next-slot), so the lowered body's refs would
            ;; point at the wrong slots.
            (let ((passes (compiler-pass-list compiler level)))
              (map (lambda (d)
                     (if (zero? level)
                       (ir->core d)
                       (ir->core (run-passes d passes))))
                   defs)))
          defs)))))

;;; optimize-on-load : syntax context -> sexp
;;; Apply the active pass pipeline to a fully-expanded PROGRAM (the
;;; (values program ctx) of compile-program-syntax), mirroring
;;; compile-defs-on-load for libraries.  It runs through syntax->ir so the
;;; <primitive-ref> / <lexical-ref> binding kinds survive the passes
;;; (compile-program's lowered output would force core->ir and lose them).
;;; This is the toplevel-script path: `load' compiles a file to one
;;; artifact and evaluates it, so the passes run here -- at eval time --
;;; instead of being baked into the artifact cache.  Level 0 or an
;;; unavailable compiler / tree-il library leaves the program lowered
;;; unoptimized.

(define (optimize-on-load program ctx)
  (let ((level (cache-level)))
    (if (zero? level)
      (lower program)
      (let ((compiler (compiler-module)))
        (if (module? compiler)
          (let ((run-passes (module-ref compiler 'run-passes))
                (ir->core (module-ref compiler 'ir->core))
                (s2ir (syntax-ir-fn)))
            (if (procedure? s2ir)
              (ir->core (run-passes (s2ir program ctx)
                                    (compiler-pass-list compiler level)))
              (lower program)))
          (lower program))))))

;;; optimize-lib-cache-recs : (list lib-cache) -> (list lib-cache)
;;; Optimize each cache record's defs at the active optimization level.  A
;;; failure to load the compiler leaves the defs untouched (optimization is
;;; optional, never a correctness requirement).  This runs ONCE at
;;; cache-write time; loading a cached library evals the already-optimized
;;; defs directly.

(define (optimize-lib-cache-recs recs)
  (map (lambda (rec)
         (let ((defs (lib-cache-defs rec)))
           (if (null? defs)
             rec
               (list (lib-cache-name rec)
                    (lib-cache-exports rec)
                    (lib-cache-imports rec)
                    (lib-cache-bindings rec)
                    (lib-cache-macros rec)
                    (compile-defs-cached defs)
                    (lib-cache-renames rec)))))
       recs))

;;; load-library-file-cached! : (list lib-cache) -> void
;;; Eval the cached defs of a file's libraries and mark them runtime-
;;; registered (the registration expression inside defs does that via
;;; runtime-registered-add!).  Dependencies referenced by module-ref in the
;;; defs are loaded first (from their own caches when available), so a
;;; cross-library value reference resolves at eval time.  The defs are
;;; already optimized for the active level (caches store optimized defs).

(define (load-library-file-cached! recs . maybe-level)
  (let ((level (registry-level-arg maybe-level)))
    (for-each (lambda (rec)
                (let ((defs (lib-cache-defs rec)))
                  (for-each (lambda (lib)
                              (if (and (not (runtime-registered? lib))
                                       (not (equal? lib (lib-cache-name rec))))
                                (load-library! lib)))
                            (apply append (map collect-cache-module-refs defs)))
                  (eval-defs defs (lib-cache-name rec) level)
                  (if (> level 0)
                    ;; No runtime module was registered (eval-defs drops
                    ;; the baked registration at level >= 1); mark the
                    ;; level-keyed instance loaded.
                    (runtime-registered-add! (lib-cache-name rec) level))))
              recs)))

;;; eval-defs : (list sexp) name [level] -> void
;;; Level 0 evaluates in the rootlet; level >= 1 in the unit inlet.

(define (eval-defs defs lib-name . maybe-level)
  (let ((level (registry-level-arg maybe-level)))
    (if (and (integer? level) (> level 0))
      ;; A level >= 1 instance registers no runtime module: the baked
      ;; (register-runtime-module ...) is dropped, since expansion-time
      ;; references resolve through inlet cells and the registration
      ;; would only clobber the level-0 module of the same name.
      (eval (cons 'begin
                  (filter (lambda (d)
                            (not (and (pair? d)
                                      (eq? (car d) 'register-runtime-module))))
                          defs))
            (current-expand-env))
      (eval (cons 'begin defs) (rootlet)))))

;;; load-library-guard : name thunk -> value
;;; Wrap a library's load/compile phase so a failure inside it (a
;;; malformed definition, an expansion error, ...) is reported with the
;;; library name and the underlying message instead of escaping as a
;;; bare `no-catch (#t)` with no location.  s7's (error msg args ...)
;;; surfaces to the handler as info = ((msg args ...) ...).  The raise
;;; below keeps template+args shape on purpose: a single-string (error s)
;;; escalates to a no-catch throw that the per-form loader re-wraps,
;;; hiding library and cause from in-process catchers.
(define (load-library-guard lib-name thunk)
  (catch
    #t
    thunk
    (lambda (tag . info)
      (let* ((detail (cond
                       ;; s7 (error msg args ...) -> info = ((msg args ...) ...)
                       ((and (pair? info)
                             (pair? (car info))
                             (or (string? (caar info)) (symbol? (caar info))))
                        (let ((msg (caar info))
                              (args (cdar info)))
                          (if (string? msg)
                            ;; Pure append: never apply format to an arbitrary
                            ;; underlying message (a ~ without matching args
                            ;; made the guard die with a second error).  Each
                            ;; arg is ~s-printed with a single-placeholder
                            ;; format, which s7 accepts for any value.
                            (if (null? args)
                              msg
                              (string-append msg (apply string-append
                                              (map (lambda (a) (format #f " ~s" a)) args))))
                            msg)))
                       ;; other raised objects (often an opaque/cyclic marker)
                       (else "malformed definition or expansion error"))))
        (error "import: failed to load library ~a: ~a" lib-name detail)))))

(define (load-library! lib-name . maybe-level)
  (let ((level (registry-level-arg maybe-level))
        (key (registry-key (registry-level-arg maybe-level) lib-name)))
    (when (member key *libraries-being-loaded*)
      (error "import: circular library dependency" lib-name))
    (let ((inlet (call-with-fresh-expand-unit
                   (lambda ()
                     (load-library-in-unit! lib-name level)
                     (and (> level 0) (current-expand-env))))))
      (when (and (> level 0) inlet)
        (instance-inlet-set! lib-name level inlet)))))

(define (load-library-in-unit! lib-name . maybe-level)
  (define level (registry-level-arg maybe-level))
  (define load-key (registry-key level lib-name))
  (let ((base (base-library)))
    (if (and base (equal? lib-name (exp-library-name base)))
      (begin
        ;; The implementation kernel (goldfish expander): not an on-disk
        ;; library -- its live bindings ARE the base library, installed by
        ;; the artifact and the boot installs (lib/install.scm).  Register
        ;; it as a record of its live bindings (the same shape library-record
        ;; builds for imports) and mark it runtime-registered, so
        ;; load-library! / import of the kernel name is a no-op instead of a
        ;; failed on-disk lookup.
        (unless (library-registry-ref lib-name)
          (library-registry-set! lib-name
            (make-lib-record base (map car (exp-library-bindings base)))))
        (unless (runtime-registered? lib-name)
          (runtime-registered-add! lib-name)))
      (let ((lib-file (library-file-name lib-name)))
        ;; cache-file-for already applies gfo-key; pass the path directly.
        (let ((gfo-file (cache-file-for lib-file)))
          (let* ((src (and (auto-compile-enabled?)
                           (load-find-module-file lib-file)))
                 ;; A libraries bundle holds one record per
                 ;; define-library form in the file, in order.
                 (payload (and src (cache-load-checked gfo-file
                                                       (compile-file-stamp src))))
                 (recs (and (bundle? payload)
                            (eq? (bundle-kind payload) 'libraries)
                            (let ((libs (bundle-section payload 'libs)))
                              (and (pair? libs) (cdr libs))))))
            (if recs
              (dynamic-wind
                (lambda ()
                  (set! *libraries-being-loaded*
                        (cons load-key *libraries-being-loaded*)))
                (lambda ()
                  (load-library-guard
                   lib-name
                   (lambda ()
                     (for-each (lambda (r) (restore-library-cache r level)) recs)
                     (load-library-file-cached! recs level))))
                (lambda ()
                  (set! *libraries-being-loaded*
                        (filter (lambda (n) (not (equal? n load-key)))
                                *libraries-being-loaded*))))
              ;; No cache (or stale): load and compile the source file.
              (let ((file (load-find-module-file lib-file)))
                (unless file
                  (error "import: unknown library" lib-name))
                (let ((forms (call-with-input-file file read-forms)))
                  (set! *perlevel-saved-records* (perlevel-snapshot-records forms))
                  (dynamic-wind
                    (lambda ()
                      (set! *libraries-being-loaded*
                            (cons load-key *libraries-being-loaded*)))
                    (lambda ()
                      (load-library-guard
                       lib-name
                       (lambda ()
                             (if (and (auto-compile-enabled?)
                                      (library-file-cacheable? forms))
                               (let* ((stamp (compile-file-stamp file))
                                      (gfo-file (cache-file-for lib-file)))
                                (let*-values (((recs ctx) (capture-file-cache forms)))
                                  (let* ((recs (optimize-lib-cache-recs recs))
                                         (deps (map library-dep-fingerprint
                                                    (library-all-deps recs lib-name))))
                                    (gfo-write! gfo-file stamp
                                                (make-bundle 'libraries (cons 'libs recs))
                                                deps)
                                    (when (> level 0)
                                      (perlevel-rebuild! recs level *perlevel-saved-records*))
                                    (load-library-file-cached! recs level))))
                           (begin
                              (let*-values (((prog ctx)
                                             (compile-program-syntax forms)))
                                (if (> level 0)
                                  (eval (optimize-on-load prog ctx) (current-expand-env))
                                  (eval (optimize-on-load prog ctx) (rootlet))))
                             (runtime-registered-add! lib-name level)
                             (when (> level 0)
                               (let ((bare (library-registry-ref lib-name)))
                                 (when bare
                                   (library-registry-set! lib-name bare level)))))))))
                    (lambda ()
                      (set! *libraries-being-loaded*
                            (filter (lambda (n) (not (equal? n load-key)))
                                    *libraries-being-loaded*)))))))))))))

;;; library-record : name -> (exp-library . exports)
;;; Look up a library record, loading the library from file on demand.

(define (library-record lib-name . maybe-level)
  (define level (registry-level-arg maybe-level))
  (let ((base (base-library)))
    (if (and base (equal? lib-name (exp-library-name base)))
        (make-lib-record base (map car (exp-library-bindings base)))
        (or (let ((rec (library-registry-ref lib-name level)))
              (and rec (runtime-registered? lib-name level) rec))
            (begin (load-library! lib-name level)
                   (library-registry-ref lib-name level))
            (error "import: unknown library" lib-name)))))

;;; ------------------------------------------------------------------------
;;; R7RS adapter
;;; ------------------------------------------------------------------------

;;; Import specs: (only lib id ...) / (except lib id ...) / (prefix lib p) /
;;; (rename lib (from to) ...) / plain library name.  An import records a
;;; SHARED view of the source library -- an interface exp-library holding the
;;; source's exported bindings, built once and reused by every importer -- on
;;; the target's uses, instead of copying the source's export table into the
;;; target's own buckets.  Resolution walks own then the uses (exp-library.scm);
;;; binding objects are shared either way, so emit and runtime linking are
;;; unchanged.

;;; The implementation library (goldfish) is imported like any other: it is
;;; not ambient, so a library (or program) body only sees names it imports.
;;; goldfish's exports are its live binding table (~830 names), so an
;;; import of it -- one shared view, reused by every importer -- exposes
;;; the substrate a system library is written against.

(define *interface-cache* '())

;;; base-lib-record : -> lib-record
;;; The implementation kernel is not an on-disk library; treat it as a record
;;; of its live bindings so imports of it work too.
(define (base-lib-record)
  (make-lib-record (base-library)
                   (map car (exp-library-bindings (base-library)))))

;;; source-record : lib-name [level] -> (exp-library . exports)
(define (source-record lib-name . maybe-level)
  (define level (registry-level-arg maybe-level))
  (if (and (base-library)
           (equal? lib-name (exp-library-name (base-library))))
    (base-lib-record)
    (library-record lib-name level)))

;;; import-view : lib-name pairs modkey strict? [level] -> view
(define (import-view lib-name pairs modkey strict? . maybe-level)
  (define level (registry-level-arg maybe-level))
  (let* ((rec (source-record lib-name level))
         (src (lib-record-library rec))
         (key (cons (cons src modkey) (cons level lib-name))))
    (let ((e (assoc key *interface-cache*)))
      (if e
        (cdr e)
        (let ((iface (make-exp-library (exp-library-name src))))
          (for-each (lambda (p)
                      (let* ((visible (car p))
                             (binding (exp-library-ref src (cdr p))))
                        (if binding
                          (let ((prior (exp-library-ref-own iface visible)))
                            (if prior
                              ;; A modifier stack that lands two DIFFERENT
                              ;; bindings on one visible name (a rename that
                              ;; collapses two exports, say) is ambiguous.
                              (if (eq? prior binding)
                                #f
                                (error "import: ~a bound more than once with different bindings (~a)"
                                       visible lib-name))
                              (exp-library-define! iface visible binding)))
                          (when strict?
                            (error "import: ~a has no binding in ~a"
                                   (cdr p) lib-name)))))
                    pairs)
          (set! *interface-cache*
                (cons (cons key iface) *interface-cache*))
          iface)))))

(define (import-into-library! lib imports)
  (for-each (lambda (spec-group)
              (for-each (lambda (spec)
                          (import-spec-into-library! lib spec))
                        spec-group))
            imports))

;;; base-sourced-binding? : binding -> boolean
;;; True for a binding that comes from the implementation substrate: a host
;;; primitive, or a toplevel whose home is the base library.  The R7RS /
;;; liii layers intentionally override substrate names, so an import may
;;; shadow a substrate-sourced binding freely.
(define (base-sourced-binding? b)
  (let ((bl (base-library)))
    (and bl
         (or (primitive-binding? b)
             (and (toplevel-binding? b)
                  (eq? (toplevel-ref-home (binding-value b)) bl))))))

;;; add-import-view! : lib lib-name iface [level] -> void
;;; Record a shared import view on lib after enforcing Racket-style import
;;; conflicts: a name already resolvable from ANY earlier import with a
;;; DIFFERENT binding that neither the implementation library nor the
;;; substrate provides is an error (R7RS: importing the same identifier more
;;; than once).  Every supplier is checked, not just the newest, so a name
;;; provided three ways is caught whenever two of them bind it differently.
;;; The implementation library freely overlaps (whichever side of an overlay
;;; it lands on), overrides of substrate bindings (host primitives /
;;; base-home toplevels) are allowed, and re-exports of the same binding are
;;; fine.  Own defines are not consulted (they may shadow an import; resolve
;;; is own-first).  A genuine peer-peer collision must be resolved with an
;;; explicit import set ((except ...), (rename ...), ...).  level is the
;;; R7RS `for' visibility level (0 = plain/run; the view is shared across
;;; importers, the level lives on the importing side's use entry).
(define (add-import-view! lib iface . maybe-level)
  (let ((level (if (pair? maybe-level) (car maybe-level) 0)))
    (let ((base-name (and (base-library)
                          (exp-library-name (base-library)))))
      (if (and base-name
               (equal? (exp-library-name iface) base-name))
        (exp-library-add-use! lib iface level)
        (let loop ((entries (exp-library-bindings iface)))
          (if (pair? entries)
            (let* ((name (caar entries))
                   (binding (cdar entries)))
              (let scan ((uses (exp-library-uses lib)))
                (cond
                  ((null? uses)
                   (loop (cdr entries)))
                  ((not (exp-library-ref-own (caar uses) name))
                   (scan (cdr uses)))
                  ((eq? (exp-library-ref-own (caar uses) name) binding)
                   ;; Same binding through this supplier (a re-export chain).
                   (scan (cdr uses)))
                  ((or (and base-name
                            (equal? (exp-library-name (caar uses)) base-name))
                       (base-sourced-binding?
                        (exp-library-ref-own (caar uses) name))
                       (base-sourced-binding? binding))
                   (scan (cdr uses)))
                  ((equal? (exp-library-name (caar uses))
                           (exp-library-name iface))
                   (scan (cdr uses)))
                  (else
                   (error "import: ~a already imported with a different binding (~a earlier vs ~a new)"
                          name
                          (exp-library-name (caar uses))
                          (exp-library-name iface))))))
            (exp-library-add-use! lib iface level)))))))

;;; R7RS import-set grammar: an <import set> is a library name or a
;;; modifier applied to a (possibly nested) <import set>:
;;;   (only <set> id ...)  (except <set> id ...)
;;;   (prefix <set> id)    (rename <set> (from to) ...)
;;; Each import set bottoms out in exactly one library.  Depth-1 sets (a
;;; modifier directly over a library name) and NESTED sets (a modifier over
;;; another modifier) share one pair-mapping core: apply-import-modifier
;;; takes a modifier kind, its args, and (visible . original) pairs to the
;;; mapped pairs.  import-set-pairs reduces a nested set to its source
;;; library plus the mapping, then resolves through import-view; it works
;;; on names only, so an only-import never pulls or resolves names it does
;;; not select.  import-view is the single collision arbiter on both paths:
;;; two visible names landing on one identifier is an error only when the
;;; bindings differ (R7RS), so a nested rename collapsing same-bound names
;;; (reachable via export aliases) behaves like its depth-1 spelling.

(define (import-set-modifier? x)
  (and (pair? x) (memq (car x) '(only except prefix rename))))

;;; apply-import-modifier : kind args (list (visible . original))
;;;                         -> (list (visible . original))
;;; The pair mapping shared by depth-1 and nested import sets.  Order
;;; preserving on every kind (the retired depth-1 except/only loops built
;;; their lists reversed; order is observable only in multi-collision
;;; error precedence).

(define (apply-import-modifier kind args pairs)
  (case kind
    ((plain) pairs)
    ((only) (filter (lambda (p) (memq (car p) args)) pairs))
    ((except) (filter (lambda (p) (not (memq (car p) args))) pairs))
    ((prefix)
     (let ((pre (car args)))
       (map (lambda (p)
              (cons (string->symbol
                     (string-append (symbol->string pre)
                                    (symbol->string (car p))))
                    (cdr p)))
            pairs)))
    ((rename)
     (map (lambda (p)
            (let ((e (assq (car p) args)))
              (if e (cons (cadr e) (cdr p)) p)))
          pairs))
    (else (error "import: bad import-set kind" kind))))

(define (import-set-pairs spec)
  (if (import-set-modifier? spec)
    (let* ((kind (car spec))
           (inner (cadr spec))
           (rest (cddr spec)))
      (call-with-values
        (lambda () (import-set-pairs inner))
        (lambda (lib-name pairs)
          (values lib-name (apply-import-modifier kind rest pairs)))))
    ;; A bare library name: its whole export list, identity-mapped.
    (values spec
            (map (lambda (n) (cons n n))
                 (lib-record-exports (source-record spec))))))

;;; import-set-view : nested-set [level] -> view/#f
(define (import-set-view spec . maybe-level)
  (define level (registry-level-arg maybe-level))
  (call-with-values
    (lambda () (import-set-pairs spec))
    (lambda (lib-name pairs)
      (if (null? pairs)
        #f
        (let ((outer (car spec)))
          (import-view lib-name pairs spec (not (eq? outer 'only)) level))))))

;;; import-level-number : level-datum -> integer
;;; R7RS import levels: run = 0, expand/syntax = 1, (meta n) = n.
;;; Visibility is "at the level and above".

(define (import-level-number level)
  (cond
    ((eq? level 'run) 0)
    ((memq level '(expand syntax)) 1)
    ((and (pair? level) (eq? (car level) 'meta)
          (pair? (cdr level)) (integer? (cadr level)))
     (cadr level))
    (else (error 'import "bad import level" level))))

;;; import-spec-level : (for set level+) -> integer
;;; Multiple levels: the minimum -- availability is "level and above",
;;; so the union of the requested levels is the lowest of them.

(define (import-spec-level spec)
  (let loop ((levels (cddr spec)) (acc #f))
    (if (null? levels)
      acc
      (loop (cdr levels)
            (let ((n (import-level-number (car levels))))
              (if acc (min acc n) n))))))

(define (import-spec-into-library! lib spec)
  (cond
    ((and (pair? spec) (eq? (car spec) 'for))
     ;; R7RS (for import-set level ...): the levels choose the phases the
     ;; import is visible at (the view is registered on the importing
     ;; library with that level; resolve-identifier consults it at phase
     ;; >= level).  The inner set bottoms out in the same library as a
     ;; plain import, so dependency loading and cache records are
     ;; unaffected.
     (if (and (pair? (cdr spec)) (pair? (cddr spec)))
         (if (and (pair? (cadr spec)) (eq? (car (cadr spec)) 'for))
             (error 'import "for spec wraps an import set, not another for" spec)
             (import-spec-clause-into-library! lib (cadr spec)
                                               (import-spec-level spec)))
         (error 'import "for spec needs an import set and at least one level" spec)))
    (else
     (import-spec-clause-into-library! lib spec 0))))

(define (import-spec-clause-into-library! lib spec level)
  (cond
    ((and (pair? spec)
          (pair? (cdr spec))
          (pair? (cadr spec))
          (import-set-modifier? (cadr spec)))
     ;; Nested import set (a modifier over another modifier).
     (let ((iface (import-set-view spec level)))
       (when iface
         (add-import-view! lib iface level))))
    ;; Depth-1 set: a modifier directly over a library name, or a bare
    ;; library name -- all through the shared pair core.
    ((and (pair? spec) (memq (car spec) '(only except prefix rename)))
     (let* ((kind (car spec))
            (args (cddr spec))
            ;; View tags keep their historical shapes (the interface
            ;; cache keys on them; content is unchanged either way).
            (tag (if (eq? kind 'prefix)
                   (cons kind (car args))
                   (cons kind args))))
       (import-depth1-into-library! lib kind (cadr spec) args tag level)))
    (else
     (import-depth1-into-library! lib 'plain spec '() 'plain level))))

;;; import-depth1-into-library! : lib kind lib-name args tag level -> void
;;; The single depth-1 handler: identity pairs over the source exports,
;;; mapped by the shared apply-import-modifier core.  An only-import
;;; selecting nothing (unknown ids) adds an empty view; skip it entirely
;;; (mirrors import-set-view's null-pairs #f).

(define (import-depth1-into-library! lib kind lib-name args tag level)
  (let ((rec (source-record lib-name level)))
    (let ((pairs (apply-import-modifier kind args
                   (map (lambda (n) (cons n n))
                        (lib-record-exports rec)))))
      (if (and (null? pairs) (eq? kind 'only))
        #t
        (add-import-view! lib
          (import-view lib-name pairs tag (not (eq? kind 'only)) level)
          level)))))

;;; define-library clause parsing: (export id ...) / (import spec ...) / body.

;;; include-file-forms : string -> (list datum)
;;; R7RS define-library `include': read the named file (resolved over
;;; *load-path*) and return its forms, to be spliced into the library body
;;; at the include clause's position.
(define (include-file-forms path)
  (let ((file (load-find-module-file path)))
    (unless file
      (error 'read-error "define-library include: file not found" path))
    (read-forms (open-input-file file))))

;;; append-map-local : (a -> (list b)) (list a) -> (list b)
;;; (SRFI-1's append-map is not in the runtime's base environment.)
(define (append-map-local f ls)
  (apply append (map f ls)))

;;; splice-includes : datum -> (list datum)
;;; R7RS `include' is a library BODY form: it may appear directly in the
;;; define-library body or inside a (begin ...) clause.  Replace every
;;; (include "file" ...) at body-element position with the file's own forms
;;; (recursively spliced; included files may themselves use include).
(define (splice-includes d)
  (cond
    ((and (pair? d) (eq? (car d) 'include))
     (append-map-local (lambda (p)
                         (append-map-local splice-includes (include-file-forms p)))
                       (cdr d)))
    ((and (pair? d) (eq? (car d) 'begin))
     (list (cons 'begin (append-map-local splice-includes (cdr d)))))
    (else (list d))))

;;; export-rename-spec? : datum -> boolean
;;; R7RS export spec (rename <from> <to>): re-export `from' under the
;;; visible name `to'.  Anything else (plain symbols, legacy malformed
;;; specs) passes through to the historical downstream errors.

(define (export-rename-spec? d)
  (and (pair? d) (eq? (car d) 'rename)
       (pair? (cdr d)) (pair? (cddr d)) (null? (cdddr d))
       (symbol? (cadr d)) (symbol? (caddr d))))

;;; split-export-specs : (list datum) -> (values (list datum) (list (to . from)))
;;; Visible export names stay in order (a rename contributes its `to');
;;; rename specs accumulate separately for alias installation.

(define (split-export-specs specs)
  (let loop ((ss specs) (ids '()) (rs '()))
    (if (null? ss)
      (values (reverse ids) (reverse rs))
      (let ((s (car ss)))
        (if (export-rename-spec? s)
          (loop (cdr ss) (cons (caddr s) ids)
                (cons (cons (caddr s) (cadr s)) rs))
          (loop (cdr ss) (cons s ids) rs))))))

(define (parse-library-clauses clauses)
  (let loop ((clauses clauses) (exports '()) (imports '()) (body '()))
    (if (null? clauses)
        (let-values (((es rs) (split-export-specs exports)))
          (values es rs (reverse imports) (reverse body)))
        (let* ((clause (syntax-form (car clauses)))
               (head (syntax->datum (car clause))))
          (cond
            ((eq? head 'export)
             (loop (cdr clauses)
                   (append exports (map syntax->datum (cdr clause)))
                   imports
                   body))
            ((eq? head 'import)
             (loop (cdr clauses)
                   exports
                   (cons (map syntax->datum (cdr clause)) imports)
                   body))
            ((eq? head 'include)
             ;; Splice include forms at this position and re-process them as
             ;; body clauses.  (splice-includes works on datums; the clauses
             ;; here are syntax trees, so convert and re-wrap.)
             (let ((spliced (map (lambda (f) (datum->syntax (car clauses) f))
                                 (splice-includes (syntax->datum clause)))))
               (loop (append spliced (cdr clauses)) exports imports body)))
            ((eq? head 'begin)
             ;; Splice internal (include ...) forms out of the begin body,
             ;; then keep the begin as one body clause (re-processing it
             ;; here would loop forever).
             (loop (cdr clauses) exports imports
                   (cons (datum->syntax (car clauses)
                                        (car (splice-includes
                                               (syntax->datum clause))))
                         body)))
            (else
             (loop (cdr clauses) exports imports (cons (car clauses) body))))))))

;;; module-form handlers (installed into the-base-library below).

;;; expand-define-library : syntax context -> (values defs ctx)
;;; Expands a define-library form, registers it, returns its defs for
;;; emission.  The defs end with a runtime module registration expression
;;; (make-module/module-define!/register-module) so the library has
;;; runtime identity; exported macros have no runtime representation.
;;; Re-exports forward through module-ref (snapshot semantics).

(define (expand-define-library stx ctx)
  (let* ((form (syntax-form stx))
         (name (syntax->datum (cadr form)))
         (clauses (cddr form)))
    (let*-values (((exports renames imports body-stxs) (parse-library-clauses clauses)))
      (let ((lib (make-exp-library name)))
        (import-into-library! lib imports)
        ;; Register BEFORE the body expands: template instantiation
        ;; (fast-instantiate) resolves a template identifier's (libref
        ;; name) back to the live library via lib-by-name/registry, and a
        ;; macro defined in this very body (whose template references this
        ;; library's imports) would otherwise find no registry entry and
        ;; degrade the identifier's library to #f -- leaving imported
        ;; names in the template unresolved.
        (library-registry-set! name (make-lib-record lib exports))
        (let ((body-stxs (map (lambda (s) (stx-set-library s lib)) body-stxs)))
          (let*-values (((defs ctx1) (expand-library-body body-stxs lib ctx)))
            ;; An exported identifier not defined in the library body must
            ;; resolve from an explicitly imported library (e.g. (scheme
            ;; base) imports (goldfish) and re-exports the host surface).
            ;; There is no implicit fallback to the base library or to a
            ;; bare host-rootlet name: the s7 dependency of every export is
            ;; declared where it is imported.  No binding at all is an
            ;; error -- the library's API must state where each name comes
            ;; from.  Exports are NOT copied into the library's own table:
            ;; importers see them through this library's shared export view,
            ;; built on demand from lib-record-exports (see import-view).
            ;; The sole exception is an export rename (rename <from> <to>),
            ;; which installs `to' as an alias of `from's binding in the
            ;; library's own table, so importers, the runtime register
            ;; expression, and the cache restore all resolve it uniformly.
            ;; Aliases install BEFORE the export check, which then validates
            ;; them like every other visible export.
            (for-each (lambda (r)
                        (let ((to (car r))
                              (from (cdr r)))
                          (let ((binding (exp-library-ref lib from)))
                            (unless binding
                              (error "define-library: export has no binding"
                                     from name))
                            (let ((prior (exp-library-ref-own lib to)))
                              (when (and prior (not (eq? prior binding)))
                                (error "define-library: export rename target already bound"
                                       to name)))
                            (exp-library-define! lib to binding)
                            (when (toplevel-binding? binding)
                              (set-toplevel-ref-exported!
                               (binding-value binding) #t)))))
                      renames)
            (for-each (lambda (export)
                        (let ((binding (exp-library-ref lib export)))
                          (unless binding
                            (error "define-library: export has no binding"
                                   export name))
                          (when (toplevel-binding? binding)
                            (set-toplevel-ref-exported! (binding-value binding) #t))))
                      exports)
            (library-registry-set! name
              (make-lib-record lib (append exports (map car renames))))
            ;; The defs are emitted as sequential top-level defines; a
            ;; forward reference (a define value naming a later define in
            ;; the same body) would unbound-error at eval time.  The host
            ;; s7 library semantics tolerate that by pre-declaring names as
            ;; #<undefined> -- previously done with (varlet (rootlet) ...
            ;; (symbol->value 'predeclare-forward-ref)).  That predeclaration
            ;; has been REMOVED (2026-08-16): it leaked two s7 host forms
            ;; into the emitted IR, and the only real user (scheme/eval's
            ;; %s7-eval) was actually a bug -- it meant the HOST eval but
            ;; captured the library's own later-defined eval as #<undefined>.
            ;; scheme/eval now resolves the host eval explicitly, and no
            ;; library relies on forward references.
            (values (append
                     defs
                     (list (datum->syntax empty-source
                             (library-register-expression lib name
                               (append exports (map car renames))))))
                    ctx1)))))))

;;; library-register-expression : exp-library name exports -> sexp
;;; (let ((m (make-module 'name)))
;;;   (module-define! m 'export ref) ...
;;;   (register-module m))

(define (library-register-expression lib name exports)
  (let ((entries
         (reverse
          (fold (lambda (acc export)
                  (let ((binding (exp-library-ref lib export)))
                    (cond
                      ((transformer-binding? binding) acc)
                      ((toplevel-binding? binding)
                       (let ((ref (binding-value binding)))
                         (cons (cons export
                                     (if (eq? (toplevel-ref-home ref) lib)
                                         (toplevel-ref-gensym ref)
                                         (list 'module-ref
                                               (list 'quote
                                                     (exp-library-name
                                                      (toplevel-ref-home ref)))
                                               (list 'quote
                                                     (toplevel-ref-original ref)))))
                               acc)))
                       ((primitive-binding? binding)
                        ;; Re-exported host primitive: the register expression
                        ;; references the primitive by its bare ambient name,
                        ;; resolved at eval time against the host rootlet (a
                        ;; name missing from the host -- as some exports of
                        ;; goldfish/scheme/base.scm are -- unbounds, which the
                        ;; catch tolerates).  Bare-name references keep the
                        ;; emitted IR free of s7 host forms (symbol->value).
                        ;; The module-define! is wrapped in a catch: a handful
                        ;; of scheme/let exports are s7 constants that cannot
                        ;; be bound (e.g. unlet -> varlet error), which s7's
                        ;; own define-library tolerates by never materializing
                        ;; them in a runtime module.
                        (cons (cons export
                                    (list 'catch
                                          '#t
                                          (list 'lambda
                                                '()
                                                (list 'module-define!
                                                      'm
                                                      (list 'quote export)
                                                      (binding-value binding)))
                                          (list 'lambda '(tag . info)
                                                '(if #f #f))))
                              acc))
                      ((or (core-form-binding? binding)
                           (module-form-binding? binding))
                       ;; Ambient syntax (lambda/if/define/...): no runtime
                       ;; value to store.
                       acc)
                      (else
                       (error "define-library: cannot export binding" export)))))
                '()
                exports))))
    ;; Built with list/append, not backquote: s7's eval of the standard
    ;; (quasiquote ...) form does not implement unquote-splicing (only its
    ;; native reader's #_list-values representation does), so backquote
    ;; templates with ,@ fail when the kernel is host-loaded through our
    ;; reader.  The datum is identical either way.  The registration is
    ;; wrapped in a self-describing form so the loader can drop it from
    ;; level >= 1 loads (see register-runtime-module).
    (list 'register-runtime-module
          (list 'quote name)
          (list 'lambda '()
                (cons 'let
                      (cons (list (list 'm (list 'make-module (list 'quote name))))
                            (append (map (lambda (entry)
                                           (let ((v (cdr entry)))
                                             (if (and (pair? v) (eq? (car v) 'catch))
                                                 ;; Primitive re-export: the entry is a
                                                 ;; full catch-wrapped module-define! (the
                                                 ;; module-define! itself may fail on an s7
                                                 ;; constant name such as unlet, so the
                                                 ;; whole call must sit inside the catch).
                                                 v
                                                 (list 'module-define! 'm
                                                       (list 'quote (car entry))
                                                       v))))
                                         entries)
                                    (list (list 'register-module 'm)
                                          (list 'runtime-registered-add!
                                                (list 'quote name))))))))))

;;; register-runtime-module : name thunk -> module
;;; The entry point baked into library artifacts (see
;;; library-register-expression).  The loader drops this form from
;;; level >= 1 loads: a level >= 1 instance has no runtime module --
;;; expansion-time references resolve through inlet cells -- so the
;;; registration would only clobber the level-0 module of the same
;;; name.  At level 0 the thunk runs unchanged.

(define (register-runtime-module name thunk)
  (thunk))

;;; expand-import : syntax context -> (values defs ctx)
;;; Top-level import: installs the imported bindings into the-base-library so
;;; subsequent top-level forms resolve them.  Emits no definitions.

(define (expand-import stx ctx)
  ;; A top-level import imports into the library the form expands against
  ;; (the syntax's library): the session PROGRAM library for expand-eval /
  ;; the REPL / compile-program-into (R7RS 5.1: a program's environment
  ;; starts empty and accumulates its imports), or the base library when a
  ;; program is compiled with compile-program (the non-cacheable library /
  ;; test-runner path).
  (let ((lib (syntax-library stx)))
    (for-each (lambda (spec)
                (import-spec-into-library! lib spec))
              (cdr (syntax->datum stx))))
  (values '() ctx))

;;; ------------------------------------------------------------------------
;;; Guile-style surface syntax
;;; ------------------------------------------------------------------------
;;; Explicit-body semantics (NOT Guile's file-level declaration):
;;;   (define-module (name ...) #:export (x y) body ...)
;;;     == (define-library (name ...) (export x y) body ...)
;;;   (use-modules spec ...) == (import spec ...)
;;; #:export is read by s7 as the keyword symbol :export.  These are
;;; module-form handlers (not plain macros): the driver dispatches
;;; module forms only on raw top-level heads, so a macro expanding to
;;; define-library/import would never be re-dispatched.

(define (expand-define-module stx ctx)
  (let* ((form (syntax-form stx))
         (name-stx (cadr form))
         (rest (cddr form)))
    (let*-values (((exports body) (parse-module-options rest)))
      (let ((clauses (if (null? exports)
                         body
                         (cons (datum->syntax stx (cons 'export exports))
                               body))))
        (expand-define-library
         (datum->syntax stx (cons 'define-library (cons name-stx clauses)))
         ctx)))))

;;; parse-module-options : (list syntax) -> (values exports body)

(define (parse-module-options rest)
  (if (and (pair? rest)
           (syntax? (car rest))
           (eq? (syntax-form (car rest)) ':export))
      (values (syntax->datum (cadr rest)) (cddr rest))
      (values '() rest)))

(define (expand-use-modules stx ctx)
  (expand-import
   (datum->syntax stx (cons 'import (cdr (syntax-form stx))))
   ctx))

;;; ------------------------------------------------------------------------
;;; Installation
;;; ------------------------------------------------------------------------

(define (install-module-forms-into! lib)
  (exp-library-define! lib 'define-library
    (make-module-form-binding expand-define-library))
  (exp-library-define! lib 'import
    (make-module-form-binding expand-import))
  (exp-library-define! lib 'define-module
    (make-module-form-binding expand-define-module))
  (exp-library-define! lib 'use-modules
    (make-module-form-binding expand-use-modules)))

(define (install-module-forms!)
  (install-module-forms-into! the-base-library))

;;; Register the module forms now.  Wrapped in a define so
;;; install-library-forms! (which only evals value definitions) runs it.
(define %module-forms-installed! (install-module-forms!))

;;; ------------------------------------------------------------------------
;;; Program libraries (R7RS 5.1)
;;; ------------------------------------------------------------------------
;;; A top-level program's environment starts EMPTY and accumulates only
;;; what the program imports: no base-library ambient fallback, and an
;;; identifier that resolves nowhere is an error (expand.scm errors when
;;; resolve-identifier returns #f inside a program library).  The program
;;; library is seeded with the core forms and the module forms (so
;;; (import ...) / (define-library ...) work) but with NO value bindings.
;;; The session-wide library (*program-lib*) is shared by expand-eval,
;;; compile-program and the REPL; --mode imports are its initial imports.

(define *program-lib* #f)

(define (make-program-library)
  (let ((lib (make-exp-library '(program))))
    (for-each (lambda (entry)
                (exp-library-define! lib (car entry)
                                     (make-core-form-binding (cdr entry))))
              (module-ref the-expander-library 'core-form-handlers))
    (install-module-forms-into! lib)
    lib))

(define (program-library)
  (or *program-lib*
      (let ((lib (make-program-library)))
        (set! *program-lib* lib)
        lib)))

(define (reset-program-library!)
  (set! *program-lib* #f)
  ;; Drop the accumulated expansion context too: it may still reference the
  ;; discarded program library's bindings.
  (set! *eval-ctx* #f))

;;; register-program-library-primitive! : symbol -> void
;;; Add a primitive binding to the session program library.  Used by the
;;; REPL for history variables ($1, $2, ...): the C layer binds them in the
;;; s7 rootlet, and the strict program environment must see them too, so a
;;; bare reference (which the host resolves in the rootlet) is registered
;;; here as a primitive binding.

(define (register-program-library-primitive! name)
  (exp-library-define! (program-library) name
                       (make-primitive-binding name)))

(define %program-library-api-installed!
  (begin
    (module-define! the-expander-library 'make-program-library make-program-library)
    (module-define! the-expander-library 'program-library program-library)
    (module-define! the-expander-library 'reset-program-library! reset-program-library!)
    (module-define! the-expander-library 'register-program-library-primitive!
                    register-program-library-primitive!)))

;;; ------------------------------------------------------------------------
;;; R7RS (scheme eval): environment / eval
;;; ------------------------------------------------------------------------
;;; An environment is an s7 inlet carrying the marker key bound to a fresh
;;; program library; the requested import-sets are imported into it (only /
;;; except / prefix / rename included, and macro transformers travel with
;;; the bindings).  eval expands the expression in that library with the
;;; Sets-of-Scopes expander and evaluates the lowered core, so macros from
;;; the environment's libraries (e.g. srfi-8's receive) work -- s7's native
;;; eval cannot.  A plain s7 environment (no marker) falls back to s7 eval.
;;;
;;; The public names are NOT installed into the base library: a base-library
;;; value binding is referenced from user code as an unresolvable install
;;; gensym (the runtime value lives in the-expander-library, invisible to
;;; rootlet-eval'd library code).  The names are instead defined in the host
;;; rootlet (like runtime-registered-add! below), so (scheme eval)'s free
;;; references resolve at runtime; they are also module-define!'d into
;;; the-expander-library for expander-internal use.

(define *program-environment-key* 'goldfish-program-environment)

(define (%make-program-environment import-sets)
  (let ((lib (make-exp-library (list 'program (gensym)))))
    (for-each (lambda (spec)
                (import-spec-into-library! lib spec))
              import-sets)
    (inlet *program-environment-key* lib)))

(define (%eval-in-program-environment expr env)
  (let ((lib (let-ref env *program-environment-key*)))
    (if (not (and (exp-library? lib) lib))
        (eval expr env)
        ;; One compilation unit per eval: expand-time state never leaks
        ;; between eval calls.
        (call-with-fresh-expand-unit
          (lambda ()
            (let* ((stx (stx-set-library (wrap-expression expr) lib))
                   (ctx (initial-context)))
              (let*-values (((defs ctx1) (expand-library-body (list stx) lib ctx)))
                (let loop ((ds defs))
                  (if (null? ds)
                      #f
                      (let ((r (eval (lower (car ds)) the-expander-library)))
                        (if (null? (cdr ds)) r (loop (cdr ds)))))))))))))

(define %environment-api-installed!
  (begin
    (module-define! the-expander-library 'make-program-environment
                    %make-program-environment)
    (module-define! the-expander-library 'eval-in-program-environment
                    %eval-in-program-environment)
    (eval (list 'define 'make-program-environment %make-program-environment)
          (rootlet))
    (eval (list 'define 'eval-in-program-environment %eval-in-program-environment)
          (rootlet))))

;;; file-import-libs : file -> (list name)
;;; Bottom libs named by every (import ...) form in a file: top-level ones
;;; (a program) and the import clauses of its define-library forms.
(define (file-import-libs file)
  (let ((forms (call-with-input-file file read-forms)))
    (let loop ((fs forms) (acc '()))
      (if (null? fs)
        acc
        (let ((f (car fs)))
          (cond
            ((and (pair? f) (eq? (car f) 'import))
             (loop (cdr fs) (append (collect-import-clause-libs f) acc)))
            ((and (pair? f) (eq? (car f) 'define-library))
             (let collect ((cs (cddr f)) (a acc))
               (if (null? cs)
                 (loop (cdr fs) a)
                 (let ((c (car cs)))
                   (if (and (pair? c) (eq? (car c) 'import))
                     (collect (cdr cs) (append (collect-import-clause-libs c) a))
                     (collect (cdr cs) a))))))
            (else (loop (cdr fs) acc))))))))

;;; file-defined-libraries : file -> (list name)
;;; Names of the libraries a file defines at top level.
(define (file-defined-libraries file)
  (let ((forms (call-with-input-file file read-forms)))
    (let loop ((fs forms) (acc '()))
      (if (null? fs)
        (reverse acc)
        (let ((f (car fs)))
          (loop (cdr fs)
                (if (and (pair? f) (eq? (car f) 'define-library)
                         (pair? (cdr f)))
                  (cons (syntax->datum (cadr f)) acc)
                  acc)))))))

;;; warm-file! : file -> (list name)
;;; Compile a program (or library) file's whole library closure into the
;;; cache -- the `gf compile` backend.  load-library! loads (and therefore
;;; compiles + caches) a library's imports recursively, so warming only
;;; needs the file's DIRECT imports plus any library it defines itself;
;;; no source-file sweep of the closure is required.  The file's own
;;; top-level forms are never evaluated, so `gf compile app.scm`
;;; precompiles everything app.scm needs and later runs are pure cache hits.
(define (warm-file! file)
  (let* ((targets (append (file-defined-libraries file)
                          (file-import-libs file))))
    (for-each (lambda (n)
                (unless (runtime-registered? n)
                  (load-library! n)))
              targets)
    targets))

;;; ------------------------------------------------------------------------
;;; Exports (wrapped in a define so install-library-forms! runs them)
;;; ------------------------------------------------------------------------

(define %module-api-exported!
  (begin
    (module-define! the-expander-library 'expand-define-library expand-define-library)
    (module-define! the-expander-library 'expand-import expand-import)
    (module-define! the-expander-library 'compile-defs-on-load compile-defs-on-load)
    (module-define! the-expander-library 'optimize-on-load optimize-on-load)
    (module-define! the-expander-library 'expand-define-module expand-define-module)
    (module-define! the-expander-library 'expand-use-modules expand-use-modules)
    (module-define! the-expander-library 'install-module-forms! install-module-forms!)
    (module-define! the-expander-library 'library-registry-ref library-registry-ref)
    (module-define! the-expander-library 'runtime-registered-add! runtime-registered-add!)
    (module-define! the-expander-library 'register-runtime-module register-runtime-module)
    (module-define! the-expander-library 'runtime-registered? runtime-registered?)
    (module-define! the-expander-library 'load-library! load-library!)
    (module-define! the-expander-library 'library-file-cacheable? library-file-cacheable?)
    (module-define! the-expander-library 'capture-library-cache capture-library-cache)
    (module-define! the-expander-library 'restore-library-cache restore-library-cache)
    (module-define! the-expander-library 'lib-record-library lib-record-library)
    (module-define! the-expander-library 'lib-record-exports lib-record-exports)
    ;; (No compat re-exports for the backend helpers that moved to
    ;; install.scm: a module-define! value is evaluated EAGERLY at install
    ;; time, when another file's source names are not rootlet-visible yet
    ;; (lib-layer defines bind gensyms in the-expander-library; source
    ;; names land in the rootlet only after each file finishes loading).
    ;; Cross-file references belong in procedure bodies (deferred), never
    ;; in top-level value position.)
    (module-define! the-expander-library 'warm-file! warm-file!)
    ;; load-library! evaluates a library's registration expression in the
    ;; host rootlet, so the runtime-registered marker and the baked
    ;; registration entry point (both called from
    ;; library-register-expression) must also be visible there.  The cached
    ;; whole-file loader (reader.scm load) also calls load-library! /
    ;; runtime-registered? to preload libraries a cached expansion refers
    ;; to, so those are exposed in the rootlet as well.
    (eval (list 'define 'runtime-registered-add! runtime-registered-add!)
          (rootlet))
    (eval (list 'define 'runtime-registered? runtime-registered?)
          (rootlet))
    (eval (list 'define 'register-runtime-module register-runtime-module)
          (rootlet))
    (eval (list 'define 'load-library! load-library!)
          (rootlet))
    (eval (list 'define 'warm-file! warm-file!)
          (rootlet))))
