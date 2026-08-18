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

;;; ---------------------------------------------------------------------------
;;; Cache substrate (used by both the macro-definition cache and the
;;; Guile-style ccache below): the ccache directory, file stamps, and the
;;; atomic cache writer.  Defined up front because the boot installs run
;;; before the ccache section.

(define (compile-cache-dir)
  (let ((xdg (getenv "XDG_CACHE_HOME")))
    (string-append
      (if (and xdg (not (string=? xdg "")))
        xdg
        (string-append (or (getenv "HOME") "/tmp") "/.cache"))
      "/goldfish/ccache")))

;;; cache-key-path : string -> string
;;; Map a source path to a filesystem-safe, NESTED cache key: the key is
;;; the source path itself (its directory structure mirrored under the
;;; cache dir), so cache artifacts are identifiable by the source path
;;; instead of an opaque hash.  "/" and "\" are treated as separators;
;;; empty and "." components are dropped; ".." is mapped to "_dotdot"
;;; (kept in the key to avoid collisions, but escaped so it cannot escape
;;; the cache directory); a drive-letter prefix ("C:") and the leading
;;; "/" of an absolute path are dropped.

(define (cache-separator? c)
  (or (char=? c #\/) (char=? c #\\)))

(define (cache-key-path path)
  (let ((n (string-length path)))
    (let loop ((i 0) (start 0) (parts '()))
      (if (> i n)
        (if (null? parts)
          "root"
          (let ((rev (reverse parts)))
            (let lp ((acc (car rev)) (rest (cdr rev)))
              (if (null? rest)
                acc
                (lp (string-append acc "/" (car rest)) (cdr rest))))))
        (if (or (= i n) (cache-separator? (string-ref path i)))
          (let ((comp (substring path start i)))
            (loop (+ i 1) (+ i 1)
                  (if (or (string=? comp "")
                          (string=? comp ".")
                          (and (> (string-length comp) 0)
                               (char=? (string-ref comp (- (string-length comp) 1)) #\:)))
                    parts
                    (cons (if (string=? comp "..") "_dotdot" comp) parts))))
          (loop (+ i 1) start parts))))))

;;; ensure-cache-parent! : dir cache-file -> void
;;; Create the cache root and every missing parent directory of cache-file
;;; under it, so nested (pathname-keyed) cache files can be written.

(define (ensure-cache-parent! dir file)
  (if (not (file-exists? dir))
    (g_mkdir dir))
  (let ((rel (substring file (string-length dir))))
    (let ((n (string-length rel)))
      (let loop ((i 1))
        (let ((j (let lp ((k i))
                   (if (or (= k n) (char=? (string-ref rel k) #\/))
                     k
                     (lp (+ k 1))))))
          (when (< j n)
            (let ((d (string-append dir (substring rel 0 j))))
              (if (not (file-exists? d))
                (g_mkdir d))
              (loop (+ j 1)))))))))

(define (compile-file-stamp path)
  (list (g_path-getmtime path) (g_path-getsize path)))

(define (compile-cache-valid? cache meta stamp)
  (and (file-exists? cache)
       (file-exists? meta)
       (let ((rec (call-with-input-file meta
                     (lambda (p) (car (read-forms p))))))
         (and (pair? rec)
              ;; Cache format version 2: install caches carry a structured
              ;; bindings field (v1 lacked it); any older cache is stale.
              ;; The meta datum is (cons (list 'compile-cache <ver>) stamp).
              (pair? (car rec))
              (equal? (cadr (car rec)) 2)
              (equal? (cdr rec) stamp)))))

(define (compile-write-cache dir cache meta stamp sexp)
  ;; Parallel test runs share this cache directory; concurrent writers
  ;; racing on the same key (tmp+rename is only per-file atomic) can leave
  ;; an inconsistent artifact behind.  GOLDFISH_CACHE_READONLY turns the
  ;; cache into read-only so parallel children never write.
  (if (getenv "GOLDFISH_CACHE_READONLY")
    #f
    (begin
      (ensure-cache-parent! dir cache)
  ;; s7's write truncates long lists at (*s7* 'print-length) (default 40);
  ;; a cached expansion easily exceeds that, so raise it for the write and
  ;; restore afterwards.  write-roundtrip (reader.scm) is used so the output
  ;; round-trips through our R7RS reader: s7's write cannot read back symbols
  ;; with special characters, and records lose their type identity.  It also
  ;; emits #n=/#n# graph labels for shared/cyclic structure and refuses
  ;; procedures (macro transformers), so a macro-defining library (which
  ;; load filters via any-macro-def?) cannot silently produce a corrupt cache.
  (let ((old-length (*s7* 'print-length)))
    (let-set! *s7* 'print-length 1000000)
    (let ((tmp (string-append cache ".tmp")))
      (call-with-output-file tmp
        (lambda (p) (write-roundtrip sexp p)))
      (g_rename tmp cache))
    (let-set! *s7* 'print-length old-length))
  (let ((mtmp (string-append meta ".tmp")))
    (call-with-output-file mtmp
      (lambda (p) (write (cons (list 'compile-cache 2) stamp) p)))
    (g_rename mtmp meta)))))

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
  (let ((file (load-find-module-file path)))
    (unless file
      (error "install-library-file!: file not found" path))
    (let ((stamp (compile-file-stamp path)))
      (if (install-cache-valid? path stamp)
        (install-cache-load! lib (install-cache-read path))
        (let*-values (((ctx defs macros bindings)
                       (install-library-forms! lib (call-with-input-file file read-forms))))
          (install-cache-save! path stamp defs macros bindings)
          ctx)))))
(define (install-standard-library!)
  (install-library-file! the-base-library "expander/lib/standard.scm"))

;;; ---------------------------------------------------------------------------
;;; Macro definition cache.
;;;
;;; A transformer is a closure and cannot be written.  Its lowered core
;;; S-expression is serializable, though: syntax objects become
;;; (stx form ctx (lib name)) -- their context is a list of (phase . scope)
;;; entries whose scopes are plain symbols (scp:N), so it round-trips
;;; through the R7RS reader/writer -- and evaluating that form again at
;;; warm start rebuilds the transformer without re-running the expander
;;; (cf. Racket's direct-eval: simple transformer expressions are likewise
;;; evaluated rather than compiled).  Value definitions are cached the same
;;; way.  The cache is invalidated by the source file's mtime and size
;;; (compile-file-stamp), same scheme as the Guile-style ccache below.

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

;;; install-cache-path : path -> (values cache meta)
(define (install-cache-path path)
  (let ((key (cache-key-path path)))
    (values (string-append (compile-cache-dir) "/" key ".mac")
            (string-append (compile-cache-dir) "/" key ".macmeta"))))

(define (install-cache-valid? path stamp)
  (let-values (((cache meta) (install-cache-path path)))
    (compile-cache-valid? cache meta stamp)))

(define (install-cache-read path)
  (let-values (((cache meta) (install-cache-path path)))
    (car (read-forms (open-input-file cache)))))

;;; compile-transformer-to-program : sexp -> datum/#f
;;; Compile a transformer's lowered form to a serialized VM bytecode
;;; program.  The compiler is a load-path library; it is looked up (never
;;; loaded here -- loading it from inside a library capture would recurse
;;; while the compiler library itself is being captured), so only
;;; libraries captured after the compiler has been loaded get bytecode
;;; transformers; anything else (notably the compiler library's own
;;; macros, and every boot macro, captured before the module system is up)
;;; keeps its lowered form as the warm-start path.
(define (compile-transformer-to-program lowered)
  (if (getenv "GOLDFISH_NO_VM_TRANSFORMER")
    #f
    (let ((compiler
           (catch #t
             (lambda ()
               ;; lookup-module is an s7 primitive, not an exp-library export,
               ;; so it is resolved as a free symbol (guarded by catch for the
               ;; bootstrap phase where the module system is not up yet).  The
               ;; compiler is preloaded by customize, so captures after that
               ;; point compile transformers to bytecode programs.
               (lookup-module '(goldfish compiler)))
             (lambda (tag . info) #f))))
      (if (module? compiler)
        (catch #t
          (lambda ()
            (let ((to-bytecode (module-ref compiler 'to-bytecode))
                  (core->ir (module-ref compiler 'core->ir)))
              (serialize-cache-sexp (to-bytecode (list (core->ir lowered))))))
          (lambda (tag . info) #f))
        #f))))

;;; install-cache-save! : path stamp (list sexp) (list (name . sexp))
;;;                      (list (original . datum)) -> void
(define (install-cache-save! path stamp defs macros bindings)
  (let-values (((cache meta) (install-cache-path path)))
    (let ((rec (list 'macro-cache 2
                     (cons 'defs (map serialize-cache-sexp defs))
                     (cons 'macros
                           (map (lambda (r)
                                  (cons (car r)
                                        (or (compile-transformer-to-program (cdr r))
                                            (serialize-cache-sexp (cdr r)))))
                                macros))
                     (cons 'bindings
                           (map (lambda (e)
                                  (cons (car e) (serialize-cache-sexp (cdr e))))
                                bindings)))))
      (compile-write-cache (compile-cache-dir) cache meta stamp rec))))

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
;;; transformers from their cached forms, registering them in the library
;;; (exp-library-define!) -- the same binding install that
;;; expand-lib-define-syntax performs, minus the re-expansion.  A cached
;;; transformer is either a serialized VM bytecode program (compiled when
;;; the compiler library was available at save time) or a lowered form;
;;; a program is loaded through vm-load (a VM closure), otherwise the form
;;; is evaluated (cf. Racket's direct-eval).

(define (install-cache-load! lib rec)
  (let ((bindings (cdr (assq 'bindings rec)))
        (defs (cdr (assq 'defs rec)))
        (macros (cdr (assq 'macros rec))))
    ;; Restore the binding table from the cached structured info (the same
    ;; (toplevel gensym home original exported?) tuples the libcache uses),
    ;; mirroring expand-lib-define-bind's exp-library-define!.  The rebuild
    ;; is inlined here (install-depurify-binding) because install-cache-load!
    ;; runs while module.scm itself is being installed, before module.scm's
    ;; depurify-binding is defined.
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
    (for-each (lambda (sexp)
                (eval (deserialize-cache-sexp sexp) the-expander-library))
              defs)
    (for-each (lambda (r)
                (let* ((name (car r))
                       (data (deserialize-cache-sexp (cdr r)))
                       (proc (if (and (pair? data) (eq? (car data) 'program))
                               (vm-load data the-expander-library)
                               (eval data the-expander-library))))
                  (exp-library-define! lib name (make-transformer-binding proc))))
              macros)))

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
;;; compile-cache-valid? / compile-write-cache are defined up top, before
;;; the boot installs.)

(define (compile-cache-hot? path stamp)
  (let ((base (string-append (compile-cache-dir) "/" (cache-key-path path))))
    (compile-cache-valid? (string-append base ".scm")
                          (string-append base ".meta")
                          stamp)))

;; ccache-level : -> integer
;; The optimization level to bake into compile-file-cached artifacts.
;; Mirrors module.scm's optimization-level, but install.scm loads before
;; module.scm, so it cannot call that procedure.

(define (ccache-level)
  (let ((v (getenv "GOLDFISH_OPT_LEVEL")))
    (cond
      ((not v) 1)
      ((member v '("0" "no" "false" "off")) 0)
      (else
       (let ((n (string->number v)))
         (if (and n (integer? n) (>= n 0)) n 1))))))

(define (compile-file-cached path)
  (let* ((key (cache-key-path path))
         (level (ccache-level))
         ;; The artifact is cached ALREADY OPTIMIZED for the active level,
         ;; so loading it again does not re-run the passes.  The level is
         ;; part of the key: level 0 keeps the plain key (unoptimized,
         ;; compatible with earlier caches), levels 1+ use a -oN suffix.
         (key (if (zero? level)
                key
                (string-append key "-o" (number->string level))))
         (dir (compile-cache-dir))
         (cache (string-append dir "/" key ".scm"))
         (meta (string-append dir "/" key ".meta"))
         (stamp (compile-file-stamp path)))
    (if (compile-cache-valid? cache meta stamp)
      (call-with-input-file cache
        (lambda (p) (car (read-forms p))))
      (let* ((sexp (compile-file path))
             (opt (if (zero? level)
                    sexp
                    (let ((f (module-ref the-expander-library 'optimize-on-load)))
                      (if (procedure? f)
                        (catch #t (lambda () (f sexp)) (lambda (type info) sexp))
                        sexp)))))
        (compile-write-cache dir cache meta stamp opt)
        opt))))

(module-define! the-expander-library 'compile-file-cached compile-file-cached)
;; The library cache (load-library! path) reuses the ccache dir, stamp,
;; validity check, and atomic writer, so expose them for lib/module.scm.
(module-define! the-expander-library 'compile-cache-dir compile-cache-dir)
(module-define! the-expander-library 'cache-key-path cache-key-path)
(module-define! the-expander-library 'ensure-cache-parent! ensure-cache-parent!)
(module-define! the-expander-library 'compile-file-stamp compile-file-stamp)
(module-define! the-expander-library 'compile-cache-valid? compile-cache-valid?)
(module-define! the-expander-library 'compile-write-cache compile-write-cache)
(module-define! the-expander-library 'compile-cache-hot? compile-cache-hot?)
;; Serializer shared with the user-library cache (lib/module.scm): a macro
;; definition caches its lowered transformer form, exactly as the boot
;; library installs do, so user libraries and the boot library build their
;; caches through one mechanism.
(module-define! the-expander-library 'serialize-cache-sexp serialize-cache-sexp)
(module-define! the-expander-library 'deserialize-cache-sexp deserialize-cache-sexp)
(module-define! the-expander-library 'compile-transformer-to-program compile-transformer-to-program)
