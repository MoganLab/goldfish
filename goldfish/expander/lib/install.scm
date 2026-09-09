;;; lib/install.scm
;;; The LIB LAYER: build the user-space macro library on top of the expander
;;; core.  This is deliberately NOT part of the expander itself (driver.scm /
;;; the pre-expanded artifact).  It is ordinary object-level source that uses
;;; the core API (wrap-expression, expand-library-body, initial-context,
;;; lower, the-base-library) as free identifiers, so it is loaded after the
;;; core is available:
;;;
;;;   - host path: driver.scm loads this at its end (so (require 'driver)
;;;     yields a working expander with the library).
;;;   - runtime path: loaded after the pre-expanded core artifact, into whose
;;;     base library these installs go.
;;;
;;; The macros themselves are closures (transformers are not serializable
;;; data), so they cannot be pre-expanded; they are installed here by running
;;; them through the expander core.  Their lowered core S-expressions ARE
;;; serializable, though -- install-library-file! caches them (see the macro
;;; definition cache below) so warm starts rebuild transformers without
;;; re-running the expander.

;; gfo helpers live in goldfish/core/gfo.scm (single source, L2 core-format)
(load-source-file "core/gfo.scm")

;; compat aliases for previous API
(define compile-cache-dir gfo-dir)
(define cache-key-path gfo-key)
(define ensure-cache-parent! gfo-ensure-parent!)
(define (compile-write-cache dir cache meta stamp sexp) (gfo-write! cache stamp sexp))

;;; ---------------------------------------------------------------------------
;;; Unified cache backend.
;;;
;;; Every expander cache entry -- a boot library install (kind `module'),
;;; a compiled toplevel program (kind `program'), a user library file
;;; (kind `libraries') -- shares one key scheme, one validity gate, and
;;; one dependency protocol:
;;;   key   : <path-mirror>[-o<level>].gfo under the versioned cache dir.
;;;           Every kind carries the optimization-level suffix (boot
;;;           installs included: optimized defs must not serve level 0).
;;;   stamp : the source content stamp (see gfo-stamp) plus the kernel
;;;           artifact stamp.
;;;   deps  : ((lib . fingerprint) ...) over the file's TRANSITIVE
;;;           import closure.  A fingerprint is (mtime size md5), or the
;;;           symbol external for sourceless libraries; fingerprints are
;;;           recomputed from the stored names on every check, so an edit
;;;           anywhere in the graph invalidates its consumers.

;;; library-file-name : lib-name -> rel-path
;;; A library name (foo bar) maps to the file "foo/bar.scm".

(define (library-file-name lib-name)
  (let loop ((parts (map symbol->string lib-name)) (acc ""))
    (if (null? parts)
        (string-append acc ".scm")
        (loop (cdr parts)
              (if (string=? acc "")
                  (car parts)
                  (string-append acc "/" (car parts)))))))

;;; library-dep-fingerprint : lib-name -> (name mtime size md5) | (name 'external)
;;; Fingerprint a dependency by its SOURCE file.  Deliberately NOT its
;;; cache artifact: consumers validate before their dependencies reload,
;;; so an artifact-based fingerprint would compare against the
;;; dependency's still-stale cache and miss the edit.  The content hash
;;; closes the same-second same-size rewrite window that mtime+size
;;; alone leave open.  Pure stat+hash calls, no expansion.

(define (library-dep-fingerprint name)
  (let ((src (load-find-module-file (library-file-name name))))
    (if src
      (cons name (list (g_path-getmtime src) (g_path-getsize src)
                       (g_md5-by-file src)))
      ;; No on-disk home: runtime-registered or host-provided library,
      ;; nothing to fingerprint.
      (cons name 'external))))

;;; import-set-lib-name : import-spec -> lib-name
;;; Bottom out of R7RS import sets: a library name, a modifier applied
;;; to a (possibly nested) set, or a `for' level spec around either.

(define (import-set-lib-name spec)
  (if (and (pair? spec)
           (memq (car spec) '(only except prefix rename for)))
    (import-set-lib-name (cadr spec))
    spec))

;;; collect-import-clause-libs : form -> (list name)
;;; Bottom library names of one (import spec ...) clause.

(define (collect-import-clause-libs clause)
  (let add ((specs (cdr clause)) (acc '()))
    (if (null? specs)
      acc
      (let ((n (import-set-lib-name (car specs))))
        (if (and (pair? n) (member n acc))
          (add (cdr specs) acc)
          (add (cdr specs) (if (pair? n) (cons n acc) acc)))))))

;;; lib-source-import-libs : file -> (list name)
;;; Bottom libs named by the (import ...) clauses of a library file's
;;; define-library forms (its direct imports, from the source --
;;; cache-free, so the closure can be walked without first restoring
;;; dependencies).

(define (lib-source-import-libs file)
  (if (not (file-exists? file))
    '()
    (let ((forms (call-with-input-file file read-forms)))
      (let loop ((fs forms) (acc '()))
        (if (null? fs)
          acc
          (let* ((f (car fs))
                 (added (if (and (pair? f) (eq? (car f) 'define-library))
                          (let collect ((cs (cddr f)) (a '()))
                            (if (null? cs)
                              a
                              (let ((c (car cs)))
                                (if (and (pair? c) (eq? (car c) 'import))
                                  (collect (cdr cs) (append (collect-import-clause-libs c) a))
                                  (collect (cdr cs) a)))))
                          '())))
            (loop (cdr fs) (append added acc))))))))

;;; transitive-lib-closure : (list name) -> (list name)
;;; BFS closure over import edges, skipping already-seen libraries.

(define (transitive-lib-closure names)
  (let loop ((queue names) (acc '()))
    (if (null? queue)
      (reverse acc)
      (let ((n (car queue)))
        (if (member n acc)
          (loop (cdr queue) acc)
          (let ((f (load-find-module-file (library-file-name n))))
            (loop (append (cdr queue)
                          (if f (lib-source-import-libs f) '()))
                  (cons n acc))))))))

;;; collect-cache-module-refs : datum -> (list name)
;;; Library names referenced as (module-ref 'lib 'name) anywhere in a
;;; cached definition or expanded program -- function or argument
;;; position: the dependencies a warm replay must have loaded before
;;; evaluating the defs.

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

;;; program-all-deps : forms opt -> (list name)
;;; Every library a compiled program can be invalidated by,
;;; transitively: the module-ref targets in the expanded output plus
;;; every library named by a top-level (import ...) form, closed over
;;; each dependency's own imports (a pure-syntax macro provider leaves
;;; no module-ref behind, and a change deep in the graph can still
;;; alter what a macro here expands to).

(define (program-all-deps forms opt)
  (transitive-lib-closure
    (dedup-libs (append (collect-cache-module-refs opt)
                        (program-import-libs forms)))))

;;; cache-level : -> integer
;;; L2-2 / program level: how much self-hosted compilation runs before
;;; defs evaluate (the -O0/1/2 convention; unset defaults to 2).
;;; Controlled by GOLDFISH_OPT_LEVEL (0 disables compilation entirely).

(define (cache-level)
  (let ((v (getenv "GOLDFISH_OPT_LEVEL")))
    (cond
      ((not v) 2)
      ((member v '("0" "no" "false" "off")) 0)
      (else
        (let ((n (string->number v)))
          (if (and n (integer? n) (>= n 0)) n 2))))))

;;; cache-level-suffix : -> string
;;; Library/program caches store defs ALREADY OPTIMIZED for the active
;;; level, so the level is part of the key.  Level 0 keeps the plain key.

(define (cache-level-suffix)
  (let ((level (cache-level)))
    (if (zero? level) "" (string-append "-o" (number->string level)))))

;;; cache-file-for : path -> gfo-file
;;; The single key builder for every cache kind: the source path
;;; mirrored under the versioned cache dir, suffixed by level.

(define (cache-file-for path)
  (string-append (compile-cache-dir) "/" (cache-key-path path)
                 (cache-level-suffix) ".gfo"))

;;; cache-load-checked : gfo-file stamp -> payload/#f
;;; The single validity gate for every cache kind: envelope shape,
;;; format version, source stamp, then the stored dependency
;;; fingerprints (recomputed from the dependency sources).  Callers
;;; check the bundle kind and extract their sections.

(define (cache-load-checked gfo-file stamp)
  (let ((rec (gfo-load-record gfo-file)))
    (and (pair? rec) (eq? (car rec) 'gfo)
         (equal? (cadr rec) gfo-format-version)
         (equal? (caddr rec) stamp)
         (let ((stored-deps (if (> (length rec) 4) (list-ref rec 4) '())))
           ;; #f is the pre-dep-protocol marker gfo-write! stores when a
           ;; writer passes no deps; treat it like an empty list.
           (and (or (not stored-deps) (null? stored-deps)
                    (and (pair? stored-deps)
                         (equal? stored-deps
                                 (map library-dep-fingerprint
                                      (map car stored-deps)))))
                (cadddr rec))))))

;; Cache stamps must cover the kernel artifact as well as the source:
;; a rebuilt artifact shifts gensym allocation, so warm-start re-eval of
;; cached macro records (whose lowered forms embed those gensyms) breaks
;; against the new boot chain even though the source never changed.
;; Memoized: one session boots with one artifact.
(define *kernel-artifact-stamp* #f)
(define (kernel-artifact-stamp)
  (or *kernel-artifact-stamp*
      (let ((artifact (or (load-find-module-file "expander/kernel-combined.scm")
                          "expander/kernel-combined.scm")))
        (set! *kernel-artifact-stamp* (gfo-stamp artifact))
        *kernel-artifact-stamp*)))
(define (compile-file-stamp path)
  (append (gfo-stamp path) (kernel-artifact-stamp)))

;;; take-collected-macros : -> (list (name . sexp))
;;; Fetch and clear the kernel's collected macro records.  Tolerates an
;;; older kernel artifact that predates the collector (returns '()).

(define (take-collected-macros)
  (if (memq 'take-macro-records (module-exports the-expander-library))
    ((module-ref the-expander-library 'take-macro-records))
    '()))

;;; install-binding-desc : binding -> datum/#f
;;; The install cache needs each value definition's (gensym home original
;;; exported?) tuple to rebuild the binding table at warm start.  This is the
;;; same extraction lib/module.scm's purify-binding performs, inlined here
;;; because install.scm loads before module.scm (and install-library-forms!
;;; is used while module.scm itself is being installed).
(define (install-binding-desc b)
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
      ((eq? kind 'transformer) 'transformer)
      (else #f))))

;;; install-library-forms! : exp-library (list datum)
;;;                        -> (values context (list sexp) (list (name . sexp))
;;;                                   (list (original . datum)))
;;; Expand a program body given as ordinary object-level source and install
;;; its definitions into the library.  Macros install at expand time; value
;;; definitions are expanded and their initializers evaluated into
;;; the-expander-library -- the module transformer code is evaluated in -- so
;;; transformer output that references a library value resolves to its
;;; runtime binding.  Returns the lowered value-definition forms, the
;;; collected macro records, and each definition's structured binding
;;; description for the cache.

(define (install-library-forms! lib forms)
  (let ((stxs (map (lambda (form)
                     (stx-set-library (wrap-expression form) lib))
                   forms)))
    (let*-values (((defs ctx)
                   (expand-library-body stxs lib (initial-context))))
      (let ((sexps (map lower defs)))
        (for-each (lambda (sexp)
                    (if (and (pair? sexp) (eq? (car sexp) 'define))
                      (eval sexp the-expander-library)
                      (error "install-library-forms!: expected value definition"
                             sexp)))
                  sexps)
        ;; Only this file's own value definitions belong in its cache, not
        ;; the whole (accumulated) library binding table.
        (let ((def-gensyms
               (filter symbol?
                       (map (lambda (s)
                              (and (pair? s) (eq? (car s) 'define) (cadr s)))
                            sexps))))
          (values ctx sexps (take-collected-macros)
                  ;; Structured binding info for the cache: each definition's
                  ;; (original . (toplevel gensym home original exported?))
                  ;; tuple, so warm start rebuilds the binding table from the
                  ;; data instead of re-deriving the original name from the
                  ;; gensym naming convention.
                  (filter (lambda (e)
                            (let ((d (cdr e)))
                              (and (pair? d)
                                   (eq? (car d) 'toplevel)
                                   (memq (cadr d) def-gensyms))))
                          (map (lambda (e)
                                 (cons (car e) (install-binding-desc (cdr e))))
                               (exp-library-bindings lib)))))))))

;;; install-library-file! : exp-library path -> context
;;; Read a file of object-level R7RS source (with the bundled reader) and
;;; expand it into the library.  File lookup reuses the loader's
;;; load-find-module-file (boot/loader.scm), the single file-finding helper.
;;; Caches the expansion (lowered value definitions plus collected macro
;;; transformer forms) under the ccache directory, keyed by the source path
;;; (its directory structure mirrored under the cache dir) and invalidated
;;; by mtime/size, so warm starts skip re-expansion.

(define (install-library-file! lib path)
  ;; One compilation unit per file: the cold expansion and the warm
  ;; replay each evaluate the file's expand-time code in a fresh env.
  (call-with-fresh-expand-unit
    (lambda ()
      (install-library-file-in-unit! lib path))))

(define (install-library-file-in-unit! lib path)
  (let ((file (load-find-module-file path)))
    (unless file
      (error "install-library-file!: file not found" path))
    (let ((stamp (compile-file-stamp path)))
      (let* ((payload (cache-load-checked (install-cache-path path) stamp))
             (cached (and (bundle? payload)
                          (eq? (bundle-kind payload) 'module)
                          payload)))
        (if cached
          (install-cache-load! lib cached)
          (let* ((forms (call-with-input-file file read-forms))
                 ;; Dependency fingerprints over the file's transitive
                 ;; import closure (empty for the boot files, which are
                 ;; plain sources with no import clauses; their capture
                 ;; is keyed by source + kernel artifact stamps).
                 (deps (map library-dep-fingerprint
                            (transitive-lib-closure
                              (program-import-libs forms)))))
            (let*-values (((ctx defs macros bindings)
                           (install-library-forms! lib forms)))
              (install-cache-save! path stamp defs macros bindings deps)
              ctx)))))))
(define (install-standard-library!)
  (install-library-file! the-base-library "expander/lib/standard.scm"))

;;; ---------------------------------------------------------------------------
;;; Cache bundles.
;;;
;;; Every expander-produced cache artifact -- a boot / user library install
;;; (kind `module') and a compiled toplevel program (kind `program') -- is
;;; one bundle record: (bundle <version> <kind> <section>*), where sections
;;; are tagged lists ((defs s*) (macros (name . s)*) (bindings ...) /
;;; (exprs s*)) and every s is serialize-cache-sexp output: syntax objects
;;; become (stx form ctx (lib name)) -- their context is a list of
;;; (phase . scope) entries whose scopes are plain symbols (scp:N), so it
;;; round-trips through the bootstrap reader -- and evaluating a stored
;;; form again at warm start rebuilds the transformer / definition without
;;; re-running the expander (cf. Racket's direct-eval: simple transformer
;;; expressions are likewise evaluated rather than compiled).  The cache is
;;; invalidated by the source file's mtime and size (compile-file-stamp),
;;; same scheme as the Guile-style ccache below.

;;; serialize-cache-sexp : any -> datum
;;; Lowered transformer forms are DAGs: the same syntax object (and the same
;;; list / vector spine) occurs many times, and a naive recursive
;;; serialization re-walks every shared subtree -- quadratic to exponential
;;; on syntax-case's transformers.  A memo (eq? -> serialized) makes each
;;; shared node serialize exactly once and REUSES the resulting object, so
;;; the writer's #n=/#n# graph labels (write-roundtrip) preserve the
;;; sharing on disk and the reader restores it as one object.

(define (serialize-cache-sexp x)
  (let ((memo '()))
    (let loop ((y x))
      (cond
        ((assq y memo) (cdr (assq y memo)))
        ((syntax? y)
         (let ((result (list 'stx* #f #f #f)))
           (set! memo (cons (cons y result) memo))
           (set-car! (cdr result) (loop (syntax-form y)))
           (set-car! (cddr result) (syntax-context y))
           (set-car! (cdddr result)
                     (list 'lib* (if (syntax-library y)
                                   (exp-library-name (syntax-library y))
                                   #f)))
           result))
        ;; Template nodes and code generated by syntax-case carry their
        ;; library as a LIVE exp-library record (its bindings hold
        ;; transformers); never walk it -- emit a serializable (lib* name)
        ;; reference instead, resolved back at load time.
        ((exp-library? y)
         (list 'lib* (exp-library-name y)))
        ;; Unserializable values raise: compile-file-cached catches and
        ;; skips the cache entry rather than writing one that cannot load.
        ((record-instance? y)
         (error "serialize-cache-sexp: cannot serialize a record" y))
        ((procedure? y)
         (error "serialize-cache-sexp: cannot serialize a procedure" y))
        ((pair? y)
         (let ((result (cons #f #f)))
           (set! memo (cons (cons y result) memo))
           (set-car! result (loop (car y)))
           (set-cdr! result (loop (cdr y)))
           result))
        ((and (vector? y) (not (bytevector? y)))
         (let* ((result (make-vector (vector-length y) #f)))
           (set! memo (cons (cons y result) memo))
           (let fill ((i 0))
             (if (< i (vector-length y))
               (begin (vector-set! result i (loop (vector-ref y i))) (fill (+ i 1)))))
           result))
        (else y)))))

;;; deserialize-lib : (lib name/#f) -> exp-library/#f
;;; Resolve a cached library reference back to the LIVE library: the base
;;; library, then the module registry (user libraries are re-registered
;;; before their cached macros are rebuilt at warm start), else a fresh
;;; (empty) exp-library as a last resort.  library-registry-ref lives in
;;; lib/module.scm, installed after this file; it is looked up through the
;;; module so bootstrap (before module.scm loads) falls back cleanly.

(define (deserialize-lib x)
  (let ((name (cadr x)))
    (cond
      ((not name) #f)
      ((and (base-library) (equal? (exp-library-name (base-library)) name))
       (base-library))
      ((and (module? the-expander-library)
            (memq 'library-registry-ref (module-exports the-expander-library)))
       (let ((rec ((module-ref the-expander-library 'library-registry-ref) name)))
         (if rec (car rec) (make-exp-library name))))
      (else (make-exp-library name)))))

;;; deserialize-cache-sexp : datum -> any
;;; Inverse of serialize-cache-sexp.  The reader already restores shared
;;; structure (write-roundtrip's #n= labels come back as one object), so a
;;; memo keyed by the serialized list rebuilds each shared syntax object
;;; exactly once.

(define (deserialize-cache-sexp x)
  (let ((memo '()))
    (let loop ((y x))
      (cond
        ((and (pair? y) (eq? (car y) 'stx*)
              (pair? (cdr y)) (pair? (cddr y)) (pair? (cdddr y)))
         (let ((cell (assq y memo)))
           (if cell
             (cdr cell)
             (let ((s (make-syntax (loop (cadr y)) (caddr y)
                                   (deserialize-lib (cadddr y)))))
               (set! memo (cons (cons y s) memo))
               s))))
        ((and (pair? y) (eq? (car y) 'lib*))
         (deserialize-lib y))
        ((pair? y) (cons (loop (car y)) (loop (cdr y))))
        ((and (vector? y) (not (bytevector? y))) (vector-map loop y))
        (else y)))))

;;; Bundle schema: (bundle <version> <kind> <section>*).  Kinds and their
;;; sections:
;;;   module    (defs s*) (macros (name . s)*) (bindings (name . s)*)
;;;             -- one boot / user library file, one library
;;;   program   (exprs s*) -- one entry per compiled program, currently the
;;;                          whole lowered form
;;;   libraries (libs rec*) -- a user library file holding one or more
;;;             define-library forms; each rec is
;;;             (name exports imports bindings macros ir-defs), where
;;;             bindings/macros are purified descriptions and ir-defs are
;;;             the file's defs already optimized for the active level
;;; Every s is serialize-cache-sexp output, so a bundle is plain text the
;;; bootstrap reader parses directly.

(define bundle-format-version 1)

(define (make-bundle kind . sections)
  (cons 'bundle
        (cons bundle-format-version
              (cons kind sections))))

(define (bundle? x)
  (and (pair? x) (eq? (car x) 'bundle)
       (equal? (cadr x) bundle-format-version)))

(define (bundle-kind x)
  (caddr x))

(define (bundle-section x tag)
  (assq tag (cdddr x)))

;;; install-cache-path : path -> gfo-file (unified .gfo)
(define (install-cache-path path) (cache-file-for path))

;;; install-cache-save! : path stamp (list sexp) (list (name . sexp))
;;;                      (list (original . datum)) -> void
(define (install-cache-save! path stamp defs macros bindings deps)
  (let ((gfo-file (install-cache-path path))
        (rec (make-bundle 'module
               (cons 'defs (map serialize-cache-sexp defs))
               (cons 'macros
                     (map (lambda (r)
                            (cons (car r) (serialize-cache-sexp (cdr r))))
                          macros))
               (cons 'bindings
                     (map (lambda (e)
                            (cons (car e) (serialize-cache-sexp (cdr e))))
                          bindings)))))
    ;; The dep list must be written explicitly: a record with no deps slot
    ;; stores #f, which the validity gate would read as a mismatch and
    ;; invalidate the entry on every check.
    (gfo-write! gfo-file stamp rec deps)))

;;; install-depurify-binding : datum exp-library -> binding/#f
;;; Rebuild a value binding from its cached description, mirroring
;;; module.scm's depurify-binding.  Inlined here because install-cache-load!
;;; runs while module.scm itself is being installed, before that procedure
;;; is defined.  home (libref name) resolves to self-lib when the binding
;;; belongs to the library being loaded; other homes go through the module
;;; registry when it is available (warm start), else #f.
(define (install-depurify-binding desc self-lib)
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
                            (or (and (defined? 'library-registry-ref)
                                     (let ((rec (library-registry-ref home-name)))
                                       (and rec (lib-record-library rec))))
                                ;; Registry unavailable (module.scm is still
                                ;; being installed) or the home is not
                                ;; registered yet: fall back to the library
                                ;; being rebuilt.  A toplevel ref with this
                                ;; home emits its gensym, which the s7
                                ;; environment binds during defs evaluation.
                                self-lib)))
                        home-desc)))
           (make-toplevel-binding
             (make-toplevel-ref gensym home original exported?))))
        ((eq? kind 'primitive)
         (make-primitive-binding (cadr desc)))
        (else #f)))))

;;; install-cache-load! : exp-library cache-datum -> void
;;; Warm start: evaluate the cached value definitions and rebuild the macro
;;; transformers from their cached lowered forms, registering them in the
;;; library (exp-library-define!) -- the same binding install that
;;; expand-lib-define-syntax performs, minus the re-expansion
;;; (cf. Racket's direct-eval).

(define (install-cache-load! lib rec)
  (let ((sections (cdddr rec)))
    (let ((bindings (cdr (assq 'bindings sections)))
          (defs (cdr (assq 'defs sections)))
          (macros (cdr (assq 'macros sections))))
    ;; Restore the binding table from the cached structured info (the same
    ;; (toplevel gensym home original exported?) tuples the libcache uses),
    ;; mirroring expand-lib-define-bind's exp-library-define!.  The rebuild
    ;; is inlined here (install-depurify-binding) because install-cache-load!
    ;; runs while module.scm itself is being installed, before module.scm's
    ;; depurify-binding is defined.
    (for-each (lambda (e)
                (let ((b (install-depurify-binding
                          (deserialize-cache-sexp (cdr e)) lib)))
                  (when b
                    (exp-library-define! lib (car e) b))))
              bindings)
    (if (null? defs)
      #f
      (eval (cons 'begin (map deserialize-cache-sexp defs)) the-expander-library))
    (for-each (lambda (r)
                (let* ((name (car r))
                       (data (deserialize-cache-sexp (cdr r)))
                       ;; Rebuild the transformer in the current unit's
                       ;; expand env (not the shared expander module),
                       ;; exactly where a cold expansion would put it.
                       (proc (eval data (current-expand-env))))
                  (exp-library-define! lib name (make-transformer-binding proc))))
              macros))))

;;; Boot: install the user-space macro layer into the base library.  Order:
;;; syntax-runtime (value definitions: pattern matching / instantiation /
;;; dispatch) and syntax-case / syntax-rules first, then the object-level
;;; define-record-type macro, then core-macros (whose syntax-rules
;;; desugaring needs syntax-case bound at phase+1), then cond-expand (uses
;;; core-macros' let / and / or), then standard.

(install-library-file! the-base-library "expander/lib/syntax-runtime.scm")
(install-library-file! the-base-library "expander/lib/syntax-case.scm")
(install-library-file! the-base-library "expander/lib/define-record-type.scm")
(install-library-file! the-base-library "expander/lib/core-macros.scm")
(install-library-file! the-base-library "expander/lib/cond-expand.scm")
;; s7 define-macro compatibility shim (depends on syntax-case).
(install-library-file! the-base-library "expander/lib/defmacro.scm")
;; s7 define* / lambda* compatibility shim (depends on syntax-case).
(install-library-file! the-base-library "expander/lib/define-star.scm")
;; The R7RS library surface (define-library/import/define-module/use-modules)
;; is self-hosted lib-layer code, not part of the core artifact; installing
;; it registers the module-form bindings in the-base-library (the trailing
;; define in lib/module.scm runs install-module-forms!).
(install-library-file! the-base-library "expander/lib/module.scm")

(module-define! the-expander-library 'install-library-forms! install-library-forms!)
(module-define! the-expander-library 'install-library-file! install-library-file!)
(module-define! the-expander-library 'install-standard-library! install-standard-library!)
;;; ---------------------------------------------------------------------------
;;; Guile-style ccache for the expander's compile: cache the expansion of a
;;; source file (the lowered core S-expression from compile-file) under
;;; $XDG_CACHE_HOME/goldfish/ccache/ (default ~/.cache/goldfish/ccache/),
;;; keyed by the source path (mirrored as nested directories), invalidated
;;; by the source's mtime and size (Guile's ccache uses the same scheme).
;;; compile-file keeps its uncached semantics; compile-file-cached is the
;;; caching entry point.
;;; (compile-cache-dir / cache-key-path / compile-file-stamp /
;;; compile-write-cache is defined up top, before
;;; the boot installs.)

;;; ccache-level : -> integer
;;; The optimization level to bake into compile-file-cached artifacts.
;;; One implementation (cache-level, defined up top with the backend);
;;; this name stays for compatibility.

(define (ccache-level) (cache-level))

;;; Program-file import deps: bottom libs named by top-level (import ...)
;;; forms (flattening begin), so macro providers are fingerprinted too.
;;; import-set-lib-name is the backend's (it also unwraps `for' specs).
(define (program-import-libs forms)
  (define (add-lib n acc)
    (if (and (pair? n) (not (member n acc))) (cons n acc) acc))
  (define (scan-specs specs acc)
    (if (null? specs)
      acc
      (scan-specs (cdr specs) (add-lib (import-set-lib-name (car specs)) acc))))
  (define (scan-form f acc)
    (if (and (pair? f) (eq? (car f) 'import))
      (scan-specs (cdr f) acc)
      (if (and (pair? f) (eq? (car f) 'begin))
        (let loop ((fs (cdr f)) (a acc))
          (if (null? fs) a (loop (cdr fs) (scan-form (car fs) a))))
        acc)))
  (let loop ((fs forms) (acc '()))
    (if (null? fs) (reverse acc) (loop (cdr fs) (scan-form (car fs) acc)))))

(define (dedup-libs ls)
  (let loop ((ls ls) (acc '()))
    (if (null? ls)
      (reverse acc)
      (if (member (car ls) acc)
        (loop (cdr ls) acc)
        (loop (cdr ls) (cons (car ls) acc))))))

(define (compile-file-cached path)
  ;; One compilation unit per call: expand-time state (region bindings,
  ;; transformer closures) is isolated from every other compile in the
  ;; session; the hot path needs no unit (it only reads data).
  (call-with-fresh-expand-unit
    (lambda ()
      (compile-file-cached-in-unit path))))

(define (compile-file-cached-in-unit path)
  (let* ((level (cache-level))
         (gfo-file (cache-file-for (cache-key-path path)))
         (stamp (compile-file-stamp path))
         (forms (call-with-input-file path read-forms)))
    ;; A program bundle holds one exprs section with the serialized
    ;; lowered program; deserialize rebuilds its embedded syntax
    ;; constants as live records.
    (let* ((payload (cache-load-checked gfo-file stamp))
           (cached (and (bundle? payload)
                        (eq? (bundle-kind payload) 'program)
                        (let ((exprs (bundle-section payload 'exprs)))
                          (and (pair? exprs)
                               (deserialize-cache-sexp (cadr exprs)))))))
      (if cached
        cached
        (let*-values (((prog ctx)
                       (compile-program-into-syntax forms
                         (program-library))))
          (let* ((opt (if (zero? level)
                          (lower prog)
                          (let ((f (module-ref the-expander-library 'optimize-on-load)))
                            (if (procedure? f)
                              (catch #t (lambda () (f prog ctx)) (lambda (type info) (lower prog)))
                              (lower prog)))))
                 ;; serialize-cache-sexp is the single arbiter of what
                 ;; persists: datum-embedded syntax values degrade to stx*
                 ;; text (their live back-reference to the session
                 ;; (program) library is replaced by its name), and
                 ;; anything unserializable raises -- such an artifact
                 ;; gets no cache entry and is re-expanded every run.
                 ;; The in-memory opt stays live either way.
                 ;; (Region bindings cannot leak here: they resolve only
                 ;; at phase >= 1, so a phase-0 artifact cannot name
                 ;; them -- a stray reference fails at expansion time.)
                 (bundle (catch #t
                           (lambda ()
                             (make-bundle 'program
                                          (list 'exprs (serialize-cache-sexp opt))))
                           (lambda args #f))))
            (when bundle
              (let ((deps (map library-dep-fingerprint
                               (program-all-deps forms opt))))
                (gfo-write! gfo-file stamp bundle deps)))
            opt))))))

(module-define! the-expander-library 'compile-file-cached compile-file-cached)
(module-define! the-expander-library 'gfo-dir gfo-dir)
(module-define! the-expander-library 'gfo-key gfo-key)
(module-define! the-expander-library 'gfo-path gfo-path)
(module-define! the-expander-library 'gfo-stamp gfo-stamp)
(module-define! the-expander-library 'gfo-valid? gfo-valid?)
(module-define! the-expander-library 'gfo-format-version gfo-format-version)
(module-define! the-expander-library 'gfo-load gfo-load)
(module-define! the-expander-library 'gfo-write! gfo-write!)
;; legacy aliases for previous API
(module-define! the-expander-library 'compile-cache-dir compile-cache-dir)
(module-define! the-expander-library 'cache-key-path cache-key-path)
(module-define! the-expander-library 'ensure-cache-parent! ensure-cache-parent!)
(module-define! the-expander-library 'compile-file-stamp compile-file-stamp)
(module-define! the-expander-library 'compile-write-cache compile-write-cache)
;; Serializer shared with the user-library cache (lib/module.scm): a macro
;; definition caches its lowered transformer form, exactly as the boot
;; library installs do, so user libraries and the boot library build their
;; caches through one mechanism.
(module-define! the-expander-library 'serialize-cache-sexp serialize-cache-sexp)
(module-define! the-expander-library 'deserialize-cache-sexp deserialize-cache-sexp)
(module-define! the-expander-library 'make-bundle make-bundle)
(module-define! the-expander-library 'bundle? bundle?)
(module-define! the-expander-library 'bundle-kind bundle-kind)
(module-define! the-expander-library 'bundle-section bundle-section)

;;; ------------------------------------------------------------------------
;;; Internal runtime surface
;;; ------------------------------------------------------------------------
;;; The reader / boot / install / module runtime functions live in the host
;;; rootlet and the-expander-library, NOT in the (goldfish) base library's
;;; binding table -- so `(import (goldfish))' does not provide them to a
;;; strict program.  Internal scripts (build-combined.scm, the tools/, the
;;; goldtest runner) are programs too and import (goldfish); register the
;;; runtime internals there as primitive bindings so those scripts resolve
;;; them.  (The reference emits the bare name, which the host rootlet /
;;; the-expander-library resolves at eval time.)

(define %internal-surface-registered!
  (for-each
    (lambda (name)
      (exp-library-define! the-base-library name (make-primitive-binding name)))
    (let ((module-forms
            ;; Names that are NOT value bindings: the core forms and module
            ;; forms stay as their real bindings in the base library; only
            ;; the runtime VALUE functions are re-registered as primitives.
            '(lambda if begin define set! quote quasiquote quote-syntax syntax
              letrec letrec* define-syntax let-syntax letrec-syntax eval-when
              define-library import define-module use-modules
              core-form-handlers)))
      (filter (lambda (name)
                (and (symbol? name)
                     (not (memq name module-forms))
                     (not (not (module-ref the-expander-library name)))))
              (module-exports the-expander-library)))))

;;; ------------------------------------------------------------------------
;;; Internal runtime surface (explicit names)
;;; ------------------------------------------------------------------------
;;; The reader / boot / install runtime functions live in the host rootlet
;;; and the-expander-library, NOT in the (goldfish) base library's binding
;;; table -- so `(import (goldfish))' does not provide them to a strict
;;; program.  Internal scripts (build-combined.scm, the tools/, the
;;; goldtest runner) are programs too and import (goldfish); register the
;;; runtime internals there as primitive bindings so those scripts resolve
;;; them.  (The reference emits the bare name, which the host rootlet /
;;; the-expander-library resolves at eval time.)

(define %internal-names-registered!
  (for-each
    (lambda (name)
      (exp-library-define! the-base-library name (make-primitive-binding name)))
    '(;; reader
      read read-forms read-line read-string read-char write-roundtrip load
      expand-eval auto-compile-enabled?
      ;; boot / loader
      load-source-file load-expanded load-find-module-file
      le-rootlet-copy
      ;; install
      install-standard-library! install-library-file! install-library-forms!
      compile-file compile-file-into compile-file-cached
      compile-cache-dir cache-key-path ensure-cache-parent!
      compile-file-stamp compile-write-cache
      cacheable-expansion? collect-module-refs
      install-cache-path install-cache-save! install-cache-load!
      ;; kernel entry points (expand-time API not already exported)
      expand expand-stx expand-library-body expand-library-finalize
      initial-context make-exp-library wrap-expression
      expand-lib-define-bind expand-lib-define-syntax
      ;; kernel exp-library / binding accessors (the base library's live
      ;; bindings hold the primitives + macros, but NOT the kernel defines
      ;; -- register them so (import (goldfish)) provides the kernel API)
      base-library set-base-library! exp-library?
      exp-library-name exp-library-bindings set-exp-library-bindings!
      exp-library-ref exp-library-define!
      exp-library-uses exp-library-ref-own exp-library-ref-at-phase
      binding? binding-kind binding-value make-binding
      lexical-binding? toplevel-binding? primitive-binding?
      transformer-binding? core-form-binding? module-form-binding?
      tstop-binding? binding-unstop make-toplevel-binding
      make-primitive-binding make-transformer-binding make-core-form-binding
      make-module-form-binding
      ;; substrate accessors not module-define!'d in the kernel
      make-record-type record-type? record-type-name record-type-fields
      record-instance? record-predicate record-accessor record-modifier
      record-field-index next-fresh next-record-rtd
      lookup-module module? make-module module-name module-ref module-define!
      context-empty context-resolve env-lookup context-env
      syntax? syntax-e syntax-form syntax-context syntax-library
      make-syntax syntax->datum datum->syntax identifier?
      free-identifier=? bound-identifier=? generate-temporaries
      make-syntax-introducer syntax-local-introduce syntax-local-value
      local-expand local-binder
      ;; s7 host forms used by the boot / install chain
      let-set! with-let sublet unlet *s7*
      the-expander-library the-base-library *base-library*
      ;; module machinery
      expand-define-library import-into-library! import-spec-into-library!
      library-registry-ref library-record load-library! load-library-file-cached!
      library-file-cacheable? capture-file-cache restore-library-cache
      capture-library-cache lib-record-library lib-record-exports
      runtime-registered-add! runtime-registered?
      make-program-library program-library reset-program-library!
      make-program-environment eval-in-program-environment)))

;;; Reader variables (*load-path*, *eval-ctx*) are REAL variables, not
;;; functions: a primitive binding would make (set! *load-path* ...) fail
;;; with "cannot assign primitive".  Register them as toplevel bindings
;;; with no home, so a reference emits the bare original name, which the
;;; host evaluator resolves to the actual variable.

(define %internal-vars-registered!
  (for-each
    (lambda (name)
      (exp-library-define! the-base-library name
                           (make-toplevel-binding
                             (make-toplevel-ref name #f name #f))))
    '(*load-path* *eval-ctx*)))
