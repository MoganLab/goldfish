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

(define (compile-file-stamp path)
  (list (g_path-getmtime path) (g_path-getsize path)))

(define (compile-cache-valid? cache meta stamp)
  (and (file-exists? cache)
       (file-exists? meta)
       (let ((rec (call-with-input-file meta
                    (lambda (p) (car (read-forms p))))))
         (and (pair? rec) (equal? (cdr rec) stamp)))))

(define (compile-write-cache dir cache meta stamp sexp)
  (if (not (file-exists? dir))
    (g_mkdir dir))
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
      (lambda (p) (write (cons 'compile-cache stamp) p)))
    (g_rename mtmp meta)))

;;; take-collected-macros : -> (list (name . sexp))
;;; Fetch and clear the kernel's collected macro records.  Tolerates an
;;; older kernel artifact that predates the collector (returns '()).

(define (take-collected-macros)
  (if (memq 'take-macro-records (module-exports the-expander-library))
    ((module-ref the-expander-library 'take-macro-records))
    '()))

;;; install-library-forms! : exp-library (list datum)
;;;                        -> (values context (list sexp) (list (name . sexp)))
;;; Expand a program body given as ordinary object-level source and install
;;; its definitions into the library.  Macros install at expand time; value
;;; definitions are expanded and their initializers evaluated into
;;; the-expander-library -- the module transformer code is evaluated in -- so
;;; transformer output that references a library value resolves to its
;;; runtime binding.  Returns the lowered value-definition forms and the
;;; collected (name . transformer-sexp) macro records so the caller can
;;; build a serializable cache.

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
        (values ctx sexps (take-collected-macros))))))

;;; install-library-file! : exp-library path -> context
;;; Read a file of object-level R7RS source (with the bundled reader) and
;;; expand it into the library.  File lookup reuses the loader's
;;; load-find-module-file (boot/loader.scm), the single file-finding helper.
;;; Caches the expansion (lowered value definitions plus collected macro
;;; transformer forms) under the ccache directory, keyed by sha256 of the
;;; path and invalidated by mtime/size, so warm starts skip re-expansion.

(define (install-library-file! lib path)
  (let ((file (load-find-module-file path)))
    (unless file
      (error "install-library-file!: file not found" path))
    (let ((stamp (compile-file-stamp path)))
      (if (install-cache-valid? path stamp)
        (install-cache-load! lib (install-cache-read path))
        (let*-values (((ctx defs macros)
                       (install-library-forms! lib (call-with-input-file file read-forms))))
          (install-cache-save! path stamp defs macros)
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
  (let ((key (g_sha256 path)))
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
  (let ((compiler
         (catch #t
           (lambda ()
             (if (and (module? the-expander-library)
                      (memq 'lookup-module (module-exports the-expander-library)))
               ((module-ref the-expander-library 'lookup-module) '(goldfish compiler))
               #f))
           (lambda (tag . info) #f))))
    (if (module? compiler)
      (catch #t
        (lambda ()
          (let ((to-bytecode (module-ref compiler 'to-bytecode))
                (core->ir (module-ref compiler 'core->ir)))
            (serialize-cache-sexp (to-bytecode (list (core->ir lowered))))))
        (lambda (tag . info) #f))
      #f)))

;;; install-cache-save! : path stamp (list sexp) (list (name . sexp)) -> void
(define (install-cache-save! path stamp defs macros)
  (let-values (((cache meta) (install-cache-path path)))
    (let ((rec (list 'macro-cache 1
                     (cons 'defs (map serialize-cache-sexp defs))
                     (cons 'macros
                           (map (lambda (r)
                                  (cons (car r)
                                        (or (compile-transformer-to-program (cdr r))
                                            (serialize-cache-sexp (cdr r)))))
                                macros)))))
      (compile-write-cache (compile-cache-dir) cache meta stamp rec))))

;;; install-cache-load! : exp-library cache-datum -> void
;;; Warm start: evaluate the cached value definitions and rebuild the macro
;;; transformers from their cached forms, registering them in the library
;;; (exp-library-define!) -- the same binding install that
;;; expand-lib-define-syntax performs, minus the re-expansion.  A cached
;;; transformer is either a serialized VM bytecode program (compiled when
;;; the compiler library was available at save time) or a lowered form;
;;; a program is loaded through vm-load (a VM closure), otherwise the form
;;; is evaluated (cf. Racket's direct-eval).

;;; gensym->original-name : symbol -> symbol
;;; context-alloc-name allocates "prefix:counter" gensyms; recover the
;;; original identifier (the prefix) so a cached definition can be
;;; registered in the library binding table under its public name.
(define (gensym->original-name sym)
  (let ((s (symbol->string sym)))
    (let loop ((i 0))
      (cond
        ((>= i (string-length s)) sym)
        ((char=? (string-ref s i) #\:) (string->symbol (substring s 0 i)))
        (else (loop (+ i 1)))))))

(define (install-cache-load! lib rec)
  (let ((defs (cdr (assq 'defs rec)))
        (macros (cdr (assq 'macros rec))))
    (for-each (lambda (sexp)
                (let ((data (deserialize-cache-sexp sexp)))
                  ;; Mirror expand-lib-define-bind's exp-library-define!:
                  ;; a cached value definition must be registered in the
                  ;; library's binding table under its original name.  The
                  ;; eval below only binds the s7 environment, which
                  ;; resolve-identifier cannot see; leaving the table empty
                  ;; makes every reference fall back to the bare original
                  ;; name, which the s7 environment does not contain on the
                  ;; warm path (e.g. syntax-case-dispatch, referenced by a
                  ;; syntax-case-generated transformer).
                  (when (and (pair? data) (eq? (car data) 'define)
                             (symbol? (cadr data)))
                    (let* ((gensym (cadr data))
                           (original (gensym->original-name gensym)))
                      (exp-library-define! lib original
                        (make-toplevel-binding
                          (make-toplevel-ref gensym lib original #f)))))
                  (eval data the-expander-library)))
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
;;; keyed by sha256 of the source path, invalidated by the source's mtime
;;; and size (Guile's ccache uses the same scheme).  compile-file keeps its
;;; uncached semantics; compile-file-cached is the caching entry point.
;;; (compile-cache-dir / compile-file-stamp / compile-cache-valid? /
;;; compile-write-cache are defined up top, before the boot installs.)

(define (compile-cache-hot? path stamp)
  (let ((base (string-append (compile-cache-dir) "/" (g_sha256 path))))
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
  (let* ((key (g_sha256 path))
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
