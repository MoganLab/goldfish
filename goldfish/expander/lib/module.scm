;;; lib/module.scm
;;; R7RS library surface as SELF-HOSTED lib-layer source (installed by
;;; lib/install.scm into the-base-library, not pre-expanded into the core
;;; artifact).  It adapts the kernel's minimal module machinery:
;;;
;;;   * Minimal module API (kernel): a module is an exp-library plus a
;;;     registry entry (exp-library . export-names).  expand-library-body
;;;     (expand/libbody.scm) expands a module body, installing its defines
;;;     into the library.  The driver knows nothing about R7RS; it only
;;;     dispatches top-level forms whose head resolves to a `module-form'
;;;     binding (see context.scm).
;;;
;;;   * Runtime modules (Guile-style, S2): define-library also emits a
;;;     registration expression (make-module/module-define!/register-module,
;;;     see common/prelude.scm) so libraries have runtime identity; cross-
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
;;; The kernel core keeps only: exp-library (module/exp-library.scm),
;;; expand-library-body (expand/libbody.scm), the binding types, and the
;;; runtime module substrate (common/prelude.scm).

;;; ------------------------------------------------------------------------
;;; Minimal module API
;;; ------------------------------------------------------------------------

;;; Registry: maps module name -> (exp-library . export-names)

(define *library-registry* '())

;;; Libraries whose runtime module (the register expression) has been
;;; evaluated.  The expand-time registry (above) is populated by
;;; expand-define-library during compilation, which may happen without the
;;; runtime registration expression ever running (a library compiled but
;;; never evaluated, e.g. by a compile-only driver).  A registration
;;; expression refers to dependencies as (module-ref 'lib 'name), which
;;; resolves against the runtime module registry -- so a library that has
;;; expand-time state but no runtime module must be loaded (evaluated)
;;; before its dependents can be registered.

(define *runtime-registered-libraries* '())

(define (runtime-registered? name)
  (member name *runtime-registered-libraries*))

(define (runtime-registered-add! name)
  (unless (member name *runtime-registered-libraries*)
    (set! *runtime-registered-libraries*
          (cons name *runtime-registered-libraries*)))
  name)

(define (library-registry-ref name)
  (let ((entry (assoc name *library-registry*)))
    (and entry (cdr entry))))

(define (library-registry-set! name record)
  (set! *library-registry*
        (cons (cons name record)
              (filter (lambda (e) (not (equal? (car e) name)))
                      *library-registry*))))

(define (make-lib-record lib exports)
  (cons lib exports))

(define (lib-record-library rec) (car rec))
(define (lib-record-exports rec) (cdr rec))

;;; On-demand file loading.  A library name (foo bar) maps to the file
;;; "foo/bar.scm" searched over *load-path* (via the loader's single
;;; load-find-module-file).  The file must contain define-library forms; it
;;; is compiled (registering the expand-time record, recursively loading
;;; imports) and evaluated (registering the runtime module).

(define (library-file-name lib-name)
  (let loop ((parts (map symbol->string lib-name)) (acc ""))
    (if (null? parts)
        (string-append acc ".scm")
        (loop (cdr parts)
              (if (string=? acc "")
                  (car parts)
                  (string-append acc "/" (car parts)))))))

;;; library-dep-fingerprint : lib-name -> (name mtime size) | (name 'external)
;;; Fingerprint a dependency's cache artifact when it exists -- so that a
;;; regenerated dependency invalidates its consumers and invalidation
;;; cascades down the import chain -- falling back to its source file.
;;; Pure stat calls, no hashing: cheap enough for every cache check.
(define (library-dep-fingerprint name)
  (let* ((lib-file (library-file-name name))
         (src (load-find-module-file lib-file))
         (gfo (and src (library-gfo-path lib-file))))
    (cond ((and gfo (file-exists? gfo))
           (cons name (list (g_path-getmtime gfo) (g_path-getsize gfo))))
          (src
           (cons name (list (g_path-getmtime src) (g_path-getsize src))))
          (else
           ;; No on-disk home: runtime-registered or host-provided library,
           ;; nothing to fingerprint.
           (cons name 'external)))))

(define (library-cache-deps recs self)
  (let loop ((ls (collect-cache-module-refs recs)) (acc '()))
    (if (null? ls)
      (reverse acc)
      (if (or (equal? (car ls) self) (member (car ls) acc))
        (loop (cdr ls) acc)
        (loop (cdr ls) (cons (car ls) acc))))))

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
;;; compiled artifact of the source, used only when the source's mtime+size
;;; still matches (the whole-file loader's ccache still serves non-library
;;; files).  A file with any non-library top-level form falls back to the
;;; previous compile-program path.

(define *libraries-being-loaded* '())

;;; Library-cache capture helpers.
;;;
;;; A library cache record is
;;;   (name exports ((id . binding-desc) ...) ((id . macro-spec) ...) defs)
;;; where defs are the lowered value/registration forms, binding-desc is the
;;; purifiable description of a value binding, and each macro-spec is the
;;; source datum of a (define-syntax id spec) body form.  On cache hit the
;;; exp-library is rebuilt from the binding descriptions, the macros are
;;; replayed with expand-lib-define-syntax, and defs are evaluated.

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

;;; library-cache-path : string -> string
;;; The ccache file for a library source file: the same key space as
;;; compile-file-cached but a distinct extension so the two kinds of cache
;;; cannot collide.  The key is the library's relative file name (e.g.
;;; "srfi/srfi-13.scm").

;;; library-cache-level-suffix : -> string
;;; Library caches are stored ALREADY OPTIMIZED for the active optimization
;;; level (see optimize-lib-cache-recs), so the level is part of the cache
;;; key: levels are kept separate and re-loading a cached library does not
;;; re-run the passes.  Level 0 keeps the plain key (unoptimized).

(define (library-cache-level-suffix)
  (let ((level (optimization-level)))
    (if (zero? level) "" (string-append "-o" (number->string level)))))

(define (library-gfo-path lib-file)
  (string-append (compile-cache-dir) "/" (cache-key-path lib-file)
                 (library-cache-level-suffix) ".gfo"))

;;; parse-define-library-body : syntax -> (values exports body-stxs)
;;; Reuse parse-library-clauses (already defined below); body stxs are the
;;; raw clause forms (export/import filtered out) so macro specs can be
;;; extracted as source datums.

;;; extract-exports : syntax -> (list symbol)

(define (extract-exports form)
  (let ((form (syntax-form form)))
    (apply append
           (map (lambda (ef)
                  (map syntax->datum (cdr (syntax-form ef))))
                (filter (lambda (cl)
                          (and (pair? (syntax-form cl))
                               (identifier? (car (syntax-form cl)))
                               (eq? (syntax-form (car (syntax-form cl))) 'export)))
                        (cddr form))))))

;;; extract-macro-specs : syntax -> (list syntax)
;;; Each macro definition body form as its SYNTAX OBJECT (not datum), so the
;;; hygienic scope information of a syntax-rules macro spec survives into the
;;; cache: re-expanding a datum spec from scratch would not reproduce the
;;; scope sets of the original definition, breaking hygiene for complex
;;; macros (e.g. liii match).  define-syntax directly, and
;;; define-macro/defmacro (s7 compatibility macros that expand to
;;; define-syntax) as their own source forms.  begin forms are spliced.

(define (extract-macro-specs form)
  (let ((form (syntax-form form)))
    (let loop ((clauses (cddr form)) (acc '()))
      (if (null? clauses)
        (reverse acc)
        (let ((clause (syntax-form (car clauses))))
          (if (not (pair? clause))
            (loop (cdr clauses) acc)
            (let ((head (car clause)))
              (cond
                ((and (identifier? head) (eq? (syntax-form head) 'begin))
                 (loop (append (cdr clause) (cdr clauses)) acc))
                ((and (identifier? head) (eq? (syntax-form head) 'cond-expand))
                 ;; Expand the cond-expand ONE macro step (scan-lib-head
                 ;; stops at the definition head), splicing the selected
                 ;; branch as (begin ...), then recurse: macros defined
                 ;; inside the selected branch (e.g. liii match's
                 ;; match-check-identifier) are top-level defines and must
                 ;; be replayed too.  Full expand-expr would descend into
                 ;; the branch and choke on the define-syntax forms.
                 (let*-values (((expanded ctx1)
                                (scan-lib-head (car clauses) (initial-context))))
                   (loop (cdr clauses)
                         (append (extract-macro-specs-of-syntax expanded)
                                 acc))))
                ((and (identifier? head) (memq (syntax-form head)
                                               '(define-syntax define-macro defmacro)))
                 (loop (cdr clauses)
                       (cons (car clauses) acc)))
                (else (loop (cdr clauses) acc))))))))))

;;; extract-macro-specs-of-syntax : syntax -> (list syntax)
;;; Extract macro definition forms from an already-expanded syntax object
;;; (e.g. the result of expanding a cond-expand), walking begin wrappers.

(define (extract-macro-specs-of-syntax stx)
  (let ((form (syntax-form stx)))
    (cond
      ((and (pair? form) (eq? (syntax-form (car form)) 'begin))
       (apply append (map extract-macro-specs-of-syntax (cdr form))))
      ((and (pair? form) (memq (syntax-form (car form))
                               '(define-syntax define-macro defmacro)))
       (list stx))
      (else '()))))

;;; purify-syntax-tree : syntax exp-library -> syntax
;;; Replace every exp-library reference in a syntax tree with a (libref name)
;;; descriptor so the tree survives write-roundtrip (a syntax record's
;;; library field points at an exp-library whose bindings table contains
;;; closures).  depurify-syntax-tree restores the references from the
;;; registry.

(define (purify-syntax-tree stx)
  (cond
    ((syntax? stx)
     (let ((form (syntax-form stx))
           (ctx (syntax-context stx))
           (lib (syntax-library stx)))
       (make-syntax
         (cond ((pair? form) (map-spine purify-syntax-tree form))
               ((vector? form) (vector-map purify-syntax-tree form))
               (else form))
         ctx
         (if (and lib (exp-library? lib))
           (list 'libref (exp-library-name lib))
           lib))))
    ((pair? stx) (cons (purify-syntax-tree (car stx)) (purify-syntax-tree (cdr stx))))
    ((vector? stx) (vector-map purify-syntax-tree stx))
    (else stx)))

(define (depurify-syntax-tree stx)
  (cond
    ((syntax? stx)
     (let ((form (syntax-form stx))
           (ctx (syntax-context stx))
           (lib (syntax-library stx)))
       (make-syntax
         (cond ((pair? form) (map-spine depurify-syntax-tree form))
               ((vector? form) (vector-map depurify-syntax-tree form))
               (else form))
         ctx
         (if (and (pair? lib) (eq? (car lib) 'libref))
           (let ((rec (library-registry-ref (cadr lib))))
             (and rec (lib-record-library rec)))
           lib))))
    ((pair? stx) (cons (depurify-syntax-tree (car stx)) (depurify-syntax-tree (cdr stx))))
    ((vector? stx) (vector-map depurify-syntax-tree stx))
    (else stx)))

;;; purify-binding : binding -> datum
;;; A serializable description of a library binding.  Value bindings
;;; (toplevel/primitive) are pure data; transformer/core-form/module-form
;;; bindings cannot be serialized (their value is a closure), so a macro
;;; binding is recorded as the symbol 'transformer and replayed from its
;;; source spec (extract-macro-specs).  A library whose bindings contain a
;;; core-form/module-form value (e.g. an exported define-library handler) is
;;; not cacheable and is signalled here.

(define (purify-binding b)
  (let ((kind (binding-kind b)))
    (cond
      ((eq? kind 'toplevel)
       (let ((ref (binding-value b)))
         (list 'toplevel
               (toplevel-ref-gensym ref)
               (let ((home (toplevel-ref-home ref)))
                 (if home (list 'libref (exp-library-name home)) #f))
               (toplevel-ref-original ref)
               (toplevel-ref-exported? ref))))
      ((eq? kind 'primitive)
       (list 'primitive (binding-value b)))
      ((eq? kind 'transformer)
       'transformer)
      (else
       (error "purify-binding: library not cacheable (unsupported binding)"
              kind)))))

;;; depurify-binding : datum exp-library -> binding/#f
;;; Rebuild a value binding from its description.  home (libref name) is
;;; resolved to the library's own record for the library itself, or the
;;; registry record for another (already loaded) library.  'transformer is
;;; #f here -- macros are replayed separately.

(define (depurify-binding desc self-lib)
  (if (eq? desc 'transformer)
    #f
    (let ((kind (car desc)))
      (cond
        ((eq? kind 'toplevel)
         (let* ((gensym (cadr desc))
                (home-desc (caddr desc))
                (original (cadddr desc))
                (exported? (car (cddddr desc)))
                (home (if (and (pair? home-desc) (eq? (car home-desc) 'libref))
                        (let ((home-name (cadr home-desc)))
                          (if (equal? home-name (exp-library-name self-lib))
                            self-lib
                            (let ((rec (library-registry-ref home-name)))
                              (and rec (lib-record-library rec)))))
                        home-desc)))
           (make-toplevel-binding (make-toplevel-ref gensym home original exported?))))
        ((eq? kind 'primitive)
         (make-primitive-binding (cadr desc)))
        (else #f)))))

;;; capture-library-cache : syntax exp-library context
;;;                             -> (values (list name exports bindings macros defs) context)
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
             (imports (let*-values (((e i b) (parse-library-clauses (cddr form))))
                        (reverse i)))
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
             ;; re-evaluates them (cf. Racket's direct-eval).  This replaces
             ;; the old source-spec replay (extract-macro-specs), which could
             ;; not recognize every macro-defining form.
             (macros (map (lambda (m)
                             (cons (car m)
                                   (or (compile-transformer-to-program (cdr m))
                                       (serialize-cache-sexp (cdr m)))))
                           (take-collected-macros)))
             (def-ir (cache-defs->ir defs ctx1)))
        (values (list name exports imports bindings macros def-ir) ctx1)))))

;;; cache-defs->ir : (list syntax) context -> (list ir|sexp)
;;; Cache the defs as record tree-il (via syntax->ir/sexp) so the
;;; optimized cache path keeps <primitive-ref> binding kinds through the
;;; passes -- the lowered form core->ir sees loses that distinction.  The
;;; IR serializes cleanly (records are plain vectors; gensym names are
;;; interned symbols).  Falls back to lowered sexp when the compiler is not
;;; loaded (bootstrap: compile-defs-cached could not ir->core it back) or
;;; when (goldfish expander tree-il) is itself being captured (recursion
;;; guard: loading it would trip the circular-dependency check).

(define (cache-defs->ir defs ctx)
  (if (or (not (runtime-registered? '(goldfish compiler)))
          (member '(goldfish expander tree-il) *libraries-being-loaded*))
    (map lower defs)
    (let ((s2ir
           (catch
             #t
             (lambda ()
               (if (not (runtime-registered? '(goldfish expander tree-il)))
                 (load-library! '(goldfish expander tree-il)))
               (module-ref (lookup-module '(goldfish expander tree-il))
                           'syntax->ir/sexp))
             (lambda (tag . info) #f))))
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
;;;   (name exports imports bindings macros defs)

(define (lib-cache-name rec) (car rec))
(define (lib-cache-exports rec) (cadr rec))
(define (lib-cache-imports rec) (caddr rec))
(define (lib-cache-bindings rec) (cadddr rec))
(define (lib-cache-macros rec) (car (cddddr rec)))
(define (lib-cache-defs rec) (cadr (cddddr rec)))

;;; restore-library-cache : lib-cache -> exp-library
;;; Rebuild a library from its cache record: re-import its dependencies
;;; (copying bindings, including re-exported macros, from their registries),
;;; restore its own value bindings, replay its macro definitions, and
;;; re-register it.  Returns the rebuilt library (defs are evaluated by the
;;; caller).

(define (restore-library-cache rec)
  (let* ((name (lib-cache-name rec))
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
                (let ((d (depurify-binding (cdr e) lib)))
                  (when d (exp-library-define! lib (car e) d))))
              bindings)
    (library-registry-set! name (make-lib-record lib exports))
    ;; 3. Rebuild this library's own macro transformers from their cached
    ;;    lowered forms: re-evaluating each form yields the transformer,
    ;;    which is registered exactly as expand-lib-define-syntax does.
    ;;    This replaces the source-spec replay (extract-macro-specs +
    ;;    expand-library-body), and is the same mechanism the boot library
    ;;    installs use -- one cache path for standard and user libraries.
    (for-each (lambda (m)
                (let* ((mname (car m))
                       (data (deserialize-cache-sexp (cdr m)))
                       (proc (if (and (pair? data) (eq? (car data) 'program))
                               (vm-load-cached-program data the-expander-library)
                               (eval data the-expander-library))))
                  (exp-library-define! lib mname (make-transformer-binding proc))))
              macros)
    ;; 4. Exports with no restored body/import binding are an error (they
    ;;    would have failed at capture time too: expand-define-library
    ;;    requires every export to resolve from the body or an import).
    (for-each (lambda (export)
                (unless (exp-library-ref lib export)
                  (error "define-library: export has no binding" export name)))
              exports)
    lib))

;;; collect-cache-module-refs : datum -> (list name)
;;; Collect library names referenced as (module-ref 'lib 'name) in a cached
;;; definition, so dependencies are loaded before the defs are evaluated.

(define (collect-cache-module-refs x)
  (let loop ((v x) (acc '()))
    (cond
      ((and (pair? v) (eq? (car v) 'module-ref))
       (let ((rest (cdr v)))
         (loop (cdr v)
               (if (and (pair? rest) (pair? (car rest)) (eq? (caar rest) 'quote))
                 (let ((lib (cadar rest)))
                   (if (member lib acc) acc (cons lib acc)))
                 acc))))
      ((pair? v) (loop (car v) (loop (cdr v) acc)))
      (else acc))))

;;; optimization-level : -> integer
;;; L2-2: how much self-hosted compilation runs on library defs before they
;;; evaluate.  The pipeline rewrites lowered core IR in place; its output is
;;; still s7-evaluable core lambda, so this changes no semantics -- only
;;; constants folded and dead if branches removed.  Levels follow the -O0/1/2
;;; convention:
;;;   0 : no compilation (defs evaluate as lowered)
;;;   1 : default -- constant folding + if simplification
;;;   2+: reserved for further passes (tail-call marking, inlining, ...)
;;; Controlled by GOLDFISH_OPT_LEVEL (0 disables compilation entirely);
;;; unset defaults to 1.

(define (optimization-level)
  (let ((v (getenv "GOLDFISH_OPT_LEVEL")))
    (cond
      ((not v) 1)
      ((member v '("0" "no" "false" "off")) 0)
      (else
       (let ((n (string->number v)))
         (if (and n (integer? n) (>= n 0)) n 1))))))

;;; compile-defs-on-load : (list syntax) context -> (list sexp)
;;; Apply the (goldfish compiler) pipeline to a library's defs.  The
;;; compiler library is loaded lazily (it is a normal load-path library, not
;;; part of the expander core), so its import must not disturb the bootstrap
;;; of the library machinery itself.  A failure to load the compiler leaves
;;; the defs lowered-but-unoptimized (compilation is an optimization, never
;;; a correctness requirement).  The active pass set grows with the
;;; optimization level; level 2+ currently enables the same core passes as
;;; level 1 and reserves the slot for future passes (tail-call marking,
;;; inlining).
;;;
;;; The defs are UN-LOWERED syntax objects (the expand-library-body
;;; output), so the pipeline runs via syntax->ir: primitive references
;;; stay <primitive-ref> nodes through the passes.  The cached-file path
;;; stores already-lowered sexp and is handled by optimize-lib-cache-recs
;;; via compile-defs (core->ir on lowered sexp).

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
  (let ((level (optimization-level)))
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
;;; path: capture-library-cache stores record tree-il via syntax->ir/sexp,
;;; falling back to lowered sexp when the compiler is unavailable).  The
;;; result is ALWAYS lowered sexp: it is what the s7 evaluator / VM eats
;;; at load time, and the cache is shared across optimization levels.
;;; IR defs run through run-passes directly (binding kinds preserved); a
;;; lowered fallback def (bootstrap capture) is lifted with core->ir.

(define (compile-defs-cached defs)
  (let ((level (optimization-level)))
    (if (and (zero? level) (or (null? defs) (not (vector? (car defs)))))
      defs
      (let ((compiler (compiler-module)))
        (if (module? compiler)
          (let ((run-passes (module-ref compiler 'run-passes))
                (core->ir (module-ref compiler 'core->ir))
                (ir->core (module-ref compiler 'ir->core)))
            ;; NOTE: lower-let is deliberately NOT in the pass set.  It
            ;; lowers a <let> into a lambda-case whose formals restart slot
            ;; numbering at 0, but syntax->ir's IR carries real lexical
            ;; (depth . index) addresses computed against the enclosing
            ;; frame (env-next-slot), so the lowered body's refs would
            ;; point at the wrong slots.  lower-let was written for
            ;; core->ir's placeholder addressing; bytecode/compile-let
            ;; handles <let> directly, so it is not needed here.
            (let ((ir-defs (if (or (null? defs) (vector? (car defs)))
                              defs
                              (map core->ir defs)))
                  (passes (compiler-pass-list compiler level)))
              (map (lambda (d)
                     (if (zero? level)
                       (ir->core d)
                       (ir->core (run-passes d passes))))
                   ir-defs)))
          defs)))))

;;; optimize-on-load : sexp -> sexp
;;; Apply the active pass pipeline to a compiled PROGRAM (a single lowered
;;; core-lambda sexp), mirroring compile-defs-on-load for libraries.  This
;;; is the toplevel-script path: `load' compiles a file to one artifact and
;;; evaluates it, so the passes run here -- at eval time -- instead of being
;;; baked into the artifact cache (which is level-independent and shared
;;; across optimization levels).  Level 0 or an unavailable compiler
;;; library leaves the program untouched.

(define (optimize-on-load sexp)
  (let ((level (optimization-level)))
    (if (zero? level)
      sexp
      (let ((compiler (compiler-module)))
        (if (module? compiler)
          (let ((run-passes (module-ref compiler 'run-passes))
                (core->ir (module-ref compiler 'core->ir))
                (ir->core (module-ref compiler 'ir->core))
                (lower-let (catch #t (lambda () (module-ref compiler 'lower-let)) (lambda _ #f))))
            ;; optimize-on-load feeds lowered core sexp (compile-program's
            ;; output), so it goes through core->ir.  lower-let is safe here
            ;; (core->ir emits placeholder addressing) and kept: the VM core
            ;; path wants let lowered to lambda/call.
            (ir->core
              (run-passes (core->ir sexp)
                          (append (compiler-pass-list compiler level)
                                  (if lower-let (list lower-let) '())))))
          sexp)))))

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
                   (compile-defs-cached defs)))))
       recs))

;;; load-library-file-cached! : (list lib-cache) -> void
;;; Eval the cached defs of a file's libraries and mark them runtime-
;;; registered (the registration expression inside defs does that via
;;; runtime-registered-add!).  Dependencies referenced by module-ref in the
;;; defs are loaded first (from their own caches when available), so a
;;; cross-library value reference resolves at eval time.  The defs are
;;; already optimized for the active level (caches store optimized defs).

(define (load-library-file-cached! recs)
  (for-each (lambda (rec)
              (let ((defs (lib-cache-defs rec)))
                ;; Preload only the dependencies of the cached defs, never
                ;; the library itself: its defs may reference it through
                ;; (module-ref '(name ...) ...) (a library's own value
                ;; definitions can refer to each other), and re-entering
                ;; load-library! for the library being loaded trips the
                ;; circular-dependency guard.
                (for-each (lambda (lib)
                            (if (and (not (runtime-registered? lib))
                                     (not (equal? lib (lib-cache-name rec))))
                              (load-library! lib)))
                          (apply append (map collect-cache-module-refs defs)))
                (vm-load-defs defs (lib-cache-name rec))))
            recs))

;;; vm-load-defs : (list sexp) name -> bool
;;; Evaluate a library's lowered defs through the VM when the (goldfish
;;; compiler) library is available; fall back to s7 eval otherwise (or when
;;; the defs do not compile).  The whole def list becomes one program (a
;;; shared code table + a sequential top), executed by one vm-load, so the
;;; definitions land in the rootlet exactly as (eval d (rootlet)) would.
;;; GOLDFISH_VM_DEFS=1 opts back into the bytecode VM.  The (goldfish
;;; compiler) library family is always exempted: once its defs are VM-
;;; itself becomes a VM closure and compiling a later library runs a nested
;;; VM compile, whose shared state produces corrupted bytecode.  Returns #t
;;; if the VM ran.

;;; vm-load-cached-program : program env -> value
;;; Transformer caches store the symbolic bytecode program; encode it
;;; into the positional vm-load ABI first.
(define (vm-load-cached-program prog env)
  (let ((compiler (lookup-module '(goldfish compiler))))
    (vm-load ((module-ref compiler 'encode-bytecode) prog) env)))

(define (vm-load-defs defs lib-name)
  ;; Default execution is plain s7 eval of the lowered core forms.  The
  ;; bytecode VM is opt-in via GOLDFISH_VM_DEFS=1: measurements
  ;; (benchmarks/measure-vm.sh) showed the cross-boundary cost outweighs
  ;; the bytecode benefit on every tracked path.
  (if (or (null? defs)
          (and (pair? lib-name) (eq? (car lib-name) 'goldfish)
               (pair? (cdr lib-name)) (eq? (cadr lib-name) 'compiler))
          (not (getenv "GOLDFISH_VM_DEFS")))
    (for-each (lambda (d) (eval d (rootlet))) defs)
    (let ((prog
           (catch
             #t
             (lambda ()
               (let ((compiler (lookup-module '(goldfish compiler))))
                 (and (module? compiler)
                      (let ((encode (module-ref compiler 'encode-bytecode))
                            (to-bytecode (module-ref compiler 'to-bytecode))
                            (core->ir (module-ref compiler 'core->ir)))
                        (encode (to-bytecode (map core->ir defs)))))))
             (lambda (tag . info) #f))))
      (if prog
        (begin (vm-load prog #f) #t)
        (begin (for-each (lambda (d) (eval d (rootlet))) defs) #f)))))

;;; load-library-guard : name thunk -> value
;;; Wrap a library's load/compile phase so a failure inside it (a
;;; malformed definition, an expansion error, ...) is reported with the
;;; library name and the underlying message instead of escaping as a
;;; bare `no-catch (#t)` with no location.  s7's (error msg args ...)
;;; surfaces to the handler as info = ((msg args ...) ...).
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
                        (if (string? (caar info))
                          (apply format #f (car info))
                          (car (car info))))
                       ;; other raised objects (often an opaque/cyclic marker)
                       (else "malformed definition or expansion error"))))
        (error "import: failed to load library ~a: ~a" lib-name detail)))))

(define (load-library! lib-name)
  (when (member lib-name *libraries-being-loaded*)
    (error "import: circular library dependency" lib-name))
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
        (set! *runtime-registered-libraries*
              (cons lib-name *runtime-registered-libraries*)))
      (let ((lib-file (library-file-name lib-name)))
        (let ((gfo-file (library-gfo-path lib-file)))
          ;; Dependency-tracked cache check: a hit requires the stored
          ;; dependency fingerprints to still match, so regenerating a
          ;; dependency invalidates its consumers.
          (let* ((src (and (auto-compile-enabled?)
                           (load-find-module-file lib-file)))
                 (rec (and src (gfo-load-record gfo-file)))
                 (recs (and (pair? rec) (eq? (car rec) 'gfo)
                            (equal? (cadr rec) gfo-format-version)
                            (equal? (caddr rec) (compile-file-stamp src))
                            (let ((deps (list-ref rec 4)))
                              (cond ((null? deps) #t)
                                    ((pair? deps)
                                     (equal? deps
                                             (map library-dep-fingerprint
                                                  (map car deps))))
                                    (else #f)))
                             (cadddr rec))))
            (if recs
              (dynamic-wind
                (lambda ()
                  (set! *libraries-being-loaded*
                        (cons lib-name *libraries-being-loaded*)))
                (lambda ()
                  (load-library-guard
                   lib-name
                   (lambda ()
                     (for-each restore-library-cache recs)
                     (load-library-file-cached! recs))))
                (lambda ()
                  (set! *libraries-being-loaded*
                        (filter (lambda (n) (not (equal? n lib-name)))
                                *libraries-being-loaded*))))
              ;; No cache (or stale): load and compile the source file.
              (let ((file (load-find-module-file lib-file)))
                (unless file
                  (error "import: unknown library" lib-name))
                (let ((forms (call-with-input-file file read-forms)))
                  (dynamic-wind
                    (lambda ()
                      (set! *libraries-being-loaded*
                            (cons lib-name *libraries-being-loaded*)))
                    (lambda ()
                      (load-library-guard
                       lib-name
                       (lambda ()
                         (if (and (auto-compile-enabled?)
                                  (library-file-cacheable? forms))
                           (let* ((stamp (compile-file-stamp file))
                                  (gfo-file (library-gfo-path lib-file)))
                             (let*-values (((recs ctx) (capture-file-cache forms)))
                               (let* ((recs (optimize-lib-cache-recs recs))
                                      (deps (map library-dep-fingerprint
                                                 (library-cache-deps recs lib-name))))
                                  (gfo-write! gfo-file stamp recs deps)
                                 (load-library-file-cached! recs))))
                           (begin
                             ;; Non-cacheable library: expand, optimize, then eval
                             ;; the whole program (the pipeline still applies).
                             (eval (optimize-on-load (compile-program forms))
                                   (rootlet))
                             (set! *runtime-registered-libraries*
                                   (cons lib-name *runtime-registered-libraries*)))))))
                    (lambda ()
                      (set! *libraries-being-loaded*
                            (filter (lambda (n) (not (equal? n lib-name)))
                                    *libraries-being-loaded*)))))))))))))

;;; library-record : name -> (exp-library . exports)
;;; Look up a library record, loading the library from file on demand.

(define (library-record lib-name)
  (let ((base (base-library)))
    (if (and base (equal? lib-name (exp-library-name base)))
        ;; (scsyntax): the implementation kernel is not an on-disk library;
        ;; treat it as a record of its live bindings so only/prefix/rename
        ;; imports of it work too.
        (make-lib-record base (map car (exp-library-bindings base)))
        (or (let ((rec (library-registry-ref lib-name)))
              (and rec (runtime-registered? lib-name) rec))
            (begin (load-library! lib-name)
                   (library-registry-ref lib-name))
            (error "import: unknown library" lib-name)))))

;;; ------------------------------------------------------------------------
;;; R7RS adapter
;;; ------------------------------------------------------------------------

;;; Import specs: (only lib id ...) / (prefix lib p) / (rename lib (from to) ...)
;;; / plain library name.  Copies exported bindings from the source library
;;; into the target library.

(define (import-into-library! lib imports)
  (for-each (lambda (spec-group)
              (for-each (lambda (spec)
                          (import-spec-into-library! lib spec))
                        spec-group))
            imports))

(define (import-spec-into-library! lib spec)
  (cond
    ((and (pair? spec) (eq? (car spec) 'only))
     (import-only-into-library! lib spec))
    ((and (pair? spec) (eq? (car spec) 'except))
     (import-except-into-library! lib spec))
    ((and (pair? spec) (eq? (car spec) 'prefix))
     (import-prefix-into-library! lib spec))
    ((and (pair? spec) (eq? (car spec) 'rename))
     (import-rename-into-library! lib spec))
    (else
     (import-plain-into-library! lib spec))))

(define (import-except-into-library! lib spec)
  (let* ((lib-name (cadr spec))
         (ids (cddr spec))
         (rec (library-record lib-name)))
    (let ((src (lib-record-library rec))
          (exports (lib-record-exports rec)))
      (for-each (lambda (name)
                  (unless (memq name ids)
                    (let ((binding (exp-library-ref src name)))
                      (when binding
                        (exp-library-define! lib name binding)))))
                exports))))

(define (import-plain-into-library! lib lib-name)
  ;; (scsyntax): the implementation kernel module is the primitive library
  ;; itself, not a registered on-disk library.  Import all of its live
  ;; bindings so the common `(import (scsyntax))' idiom works; the
  ;; r7rs-small `(import (scheme base))' loads the on-disk
  ;; scsyntax/scheme/base.scm which re-exports the standard surface.
  ;; Exports are computed at import time so base macros installed later
  ;; (standard.scm) are included.
  (let* ((base (base-library))
         (base? (and base (equal? lib-name (exp-library-name base))))
         (src (if base? base (lib-record-library (library-record lib-name))))
         (exports (if base?
                      (map car (exp-library-bindings base))
                      (lib-record-exports (library-record lib-name)))))
    (for-each (lambda (name)
                (let ((binding (exp-library-ref src name)))
                  (unless binding
                    (error "import: exported identifier has no binding" name))
                  (exp-library-define! lib name binding)))
              exports)))

(define (import-only-into-library! lib spec)
  (let* ((lib-name (cadr spec))
         (ids (cddr spec))
         (rec (library-record lib-name)))
    (let ((src (lib-record-library rec))
          (exports (lib-record-exports rec)))
      ;; Tolerate ids the source library does not export (s7's (import
      ;; (only ...)) does): scheme/time.scm imports s7-round from
      ;; (scheme base), which the goldfish scheme/base does not export but
      ;; s7's r7rs library does.
      (for-each (lambda (id)
                  (when (memq id exports)
                    (let ((binding (exp-library-ref src id)))
                      (when binding
                        (exp-library-define! lib id binding)))))
                ids))))

(define (import-prefix-into-library! lib spec)
  (let* ((lib-name (cadr spec))
         (prefix (caddr spec))
         (rec (library-record lib-name)))
    (let ((src (lib-record-library rec))
          (exports (lib-record-exports rec)))
      (for-each (lambda (name)
                  (let ((binding (exp-library-ref src name)))
                    (unless binding
                      (error "import: exported identifier has no binding" name))
                    (let ((prefixed (string->symbol
                                     (string-append (symbol->string prefix)
                                                    (symbol->string name)))))
                      (exp-library-define! lib prefixed binding))))
                exports))))

(define (import-rename-into-library! lib spec)
  (let* ((lib-name (cadr spec))
         (renames (cddr spec))
         (rec (library-record lib-name)))
    (let ((src (lib-record-library rec))
          (exports (lib-record-exports rec)))
      (for-each (lambda (name)
                  (let ((binding (exp-library-ref src name)))
                    (unless binding
                      (error "import: exported identifier has no binding" name))
                    (let ((rename-entry (assq name renames)))
                      (if rename-entry
                          (exp-library-define! lib (cadr rename-entry) binding)
                          (exp-library-define! lib name binding)))))
                exports))))

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

(define (parse-library-clauses clauses)
  (let loop ((clauses clauses) (exports '()) (imports '()) (body '()))
    (if (null? clauses)
        (values exports (reverse imports) (reverse body))
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
    (let*-values (((exports imports body-stxs) (parse-library-clauses clauses)))
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
            ;; An exported identifier not defined in the library body is
            ;; inherited from the base library when the base library has it
            ;; (host primitives / core forms / ambient syntax re-exported
            ;; without a body definition, as goldfish/scheme/base.scm does);
            ;; otherwise it falls back to a host primitive reference resolved
            ;; at eval time (the scheme base library exports the full s7
            ;; r7rs procedure set, most of which is never defined in its
            ;; body).  No binding at all is NOT an error: the host s7
            ;; environment loads scheme/base.scm with the same tolerance.
            ;; An exported identifier not defined in the library body must
            ;; resolve from an explicitly imported library (e.g. (scheme
            ;; base) imports (goldfish) and re-exports the host surface).
            ;; There is no implicit fallback to the base library or to a
            ;; bare host-rootlet name: the s7 dependency of every export is
            ;; declared where it is imported.  No binding at all is an
            ;; error -- the library's API must state where each name comes
            ;; from.
            (for-each (lambda (export)
                        (let ((binding (exp-library-ref lib export)))
                          (unless binding
                            (error "define-library: export has no binding"
                                   export name))
                          (exp-library-define! lib export binding)
                          (when (toplevel-binding? binding)
                            (set-toplevel-ref-exported! (binding-value binding) #t))))
                      exports)
            (library-registry-set! name (make-lib-record lib exports))
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
                     (list (datum->syntax lib-output-source
                             (library-register-expression lib name exports))))
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
    ;; reader.  The datum is identical either way.
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
                                    (list 'quote name))))))))

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
        (let* ((stx (stx-set-library (wrap-expression expr) lib))
               (ctx (initial-context)))
          (let*-values (((defs ctx1) (expand-library-body (list stx) lib ctx)))
            (let loop ((ds defs))
              (if (null? ds)
                  #f
                  (let ((r (eval (lower (car ds)) the-expander-library)))
                    (if (null? (cdr ds)) r (loop (cdr ds)))))))))))

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
    (module-define! the-expander-library 'runtime-registered? runtime-registered?)
    (module-define! the-expander-library 'load-library! load-library!)
    (module-define! the-expander-library 'library-file-cacheable? library-file-cacheable?)
    (module-define! the-expander-library 'capture-library-cache capture-library-cache)
    (module-define! the-expander-library 'restore-library-cache restore-library-cache)
    (module-define! the-expander-library 'lib-record-library lib-record-library)
    (module-define! the-expander-library 'lib-record-exports lib-record-exports)
    (module-define! the-expander-library 'collect-cache-module-refs collect-cache-module-refs)
    (module-define! the-expander-library 'library-dep-fingerprint library-dep-fingerprint)
    ;; load-library! evaluates a library's registration expression in the
    ;; host rootlet, so the runtime-registered marker (called from
    ;; library-register-expression) must also be visible there.  The cached
    ;; whole-file loader (reader.scm load) also calls load-library! /
    ;; runtime-registered? to preload libraries a cached expansion refers
    ;; to, so those are exposed in the rootlet as well.
    (eval (list 'define 'runtime-registered-add! runtime-registered-add!)
          (rootlet))
    (eval (list 'define 'runtime-registered? runtime-registered?)
          (rootlet))
    (eval (list 'define 'load-library! load-library!)
          (rootlet))))
