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

(define (gfo-dir)
  (let ((xdg (getenv "XDG_CACHE_HOME")))
    (string-append
      (if (and xdg (not (string=? xdg "")))
        xdg
        (string-append (or (getenv "HOME") "/tmp") "/.cache"))
      "/goldfish/ccache")))

(define (gfo-separator? c)
  (or (char=? c #\/) (char=? c #\\)))

(define (gfo-key path)
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
        (if (or (= i n) (gfo-separator? (string-ref path i)))
          (let ((comp (substring path start i)))
            (loop (+ i 1) (+ i 1)
                  (if (or (string=? comp "")
                          (string=? comp ".")
                          (and (> (string-length comp) 0)
                               (char=? (string-ref comp (- (string-length comp) 1)) #\:)))
                    parts
                    (cons (if (string=? comp "..") "_dotdot" comp) parts))))
          (loop (+ i 1) start parts))))))

(define (gfo-path path)
  (string-append (gfo-dir) "/" (gfo-key path) ".gfo"))

(define (gfo-ensure-parent! dir file)
  (if (not (file-exists? dir)) (g_mkdir dir))
  (let ((rel (substring file (string-length dir))))
    (let ((n (string-length rel)))
      (let loop ((i 1))
        (let ((j (let lp ((k i))
                   (if (or (= k n) (char=? (string-ref rel k) #\/))
                     k (lp (+ k 1))))))
          (when (< j n)
            (let ((d (string-append dir (substring rel 0 j))))
              (if (not (file-exists? d)) (g_mkdir d))
              (loop (+ j 1)))))))))

(define (gfo-stamp path)
  (list (g_path-getmtime path) (g_path-getsize path)))

(define (gfo-valid? gfo-file stamp)
  (and (file-exists? gfo-file)
       (let ((rec (call-with-input-file gfo-file (lambda (p) (car (read-forms p))))))
         (and (pair? rec) (eq? (car rec) 'gfo) (equal? (cadr rec) stamp)))))

(define (gfo-read gfo-file)
  (let ((rec (car (read-forms (open-input-file gfo-file)))))
    (caddr rec)))

(define (gfo-write! gfo-file stamp payload)
  (if (getenv "GOLDFISH_CACHE_READONLY") #f
      (begin
        (gfo-ensure-parent! (gfo-dir) gfo-file)
        (let ((old-length (*s7* 'print-length)))
          (let-set! *s7* 'print-length 1000000)
          (let ((tmp (string-append gfo-file ".tmp")))
            (call-with-output-file tmp
              (lambda (p)
                (if (defined? 'write-roundtrip) (write-roundtrip (list 'gfo stamp payload) p)
                    (write (list 'gfo stamp payload) p))))
            (g_rename tmp gfo-file))
          (let-set! *s7* 'print-length old-length)))))

;; compat aliases for previous API
(define compile-cache-dir gfo-dir)
(define cache-key-path gfo-key)
(define cache-separator? gfo-separator?)
(define ensure-cache-parent! gfo-ensure-parent!)
(define compile-file-stamp gfo-stamp)
(define (compile-cache-valid? cache meta stamp) (gfo-valid? cache stamp))
(define (compile-write-cache dir cache meta stamp sexp) (gfo-write! cache stamp sexp))

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

;;; install-cache-path : path -> gfo-file (unified .gfo)
(define (install-cache-path path) (gfo-path path))

(define (install-cache-valid? path stamp)
  (gfo-valid? (install-cache-path path) stamp))

(define (install-cache-read path)
  (gfo-read (install-cache-path path)))

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
  ;; Compile a transformer's lowered form to a serialized VM bytecode
  ;; program (the fast warm-start path).  The VM interpreter has its own
  ;; apply handling (call_function splices the final list argument of
  ;; (apply ...) directly and calls the procedure, bypassing s7's deferred
  ;; apply opcode), so define-macro transformers whose body applies a
  ;; closure work from the cache.  GOLDFISH_NO_VM_TRANSFORMER=1 falls back
  ;; to the lowered-form path (serialize-cache-sexp + eval).
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
  (let ((gfo-file (install-cache-path path))
        (rec (list 'macro-cache 2
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
    (gfo-write! gfo-file stamp rec)))

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
  (let* ((key (cache-key-path path))
         (level (ccache-level))
         (key (if (zero? level) key (string-append key "-o" (number->string level))))
         (gfo-file (string-append (compile-cache-dir) "/" key ".gfo")))
    (gfo-valid? gfo-file stamp)))

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
         (key (if (zero? level) key (string-append key "-o" (number->string level))))
         (gfo-file (string-append (compile-cache-dir) "/" key ".gfo"))
         (stamp (compile-file-stamp path)))
    (if (gfo-valid? gfo-file stamp)
      (gfo-read gfo-file)
      (let* ((sexp (compile-file-into path (program-library)))
             (opt (if (zero? level)
                    sexp
                    (let ((f (module-ref the-expander-library 'optimize-on-load)))
                      (if (procedure? f)
                        (catch #t (lambda () (f sexp)) (lambda (type info) sexp))
                        sexp)))))
        (gfo-write! gfo-file stamp opt)
        opt))))

(module-define! the-expander-library 'compile-file-cached compile-file-cached)
(module-define! the-expander-library 'gfo-dir gfo-dir)
(module-define! the-expander-library 'gfo-key gfo-key)
(module-define! the-expander-library 'gfo-path gfo-path)
(module-define! the-expander-library 'gfo-stamp gfo-stamp)
(module-define! the-expander-library 'gfo-valid? gfo-valid?)
(module-define! the-expander-library 'gfo-read gfo-read)
(module-define! the-expander-library 'gfo-write! gfo-write!)
;; legacy aliases for previous API
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
      expand-eval eval-forms auto-compile-enabled?
      ;; boot / loader
      load-source-file load-expanded load-find-module-file
      le-cache-files le-cache-valid? le-write-cache le-rootlet-copy
      bootstrap-macros-installed?
      ;; install
      install-standard-library! install-library-file! install-library-forms!
      compile-file compile-file-into compile-file-cached
      compile-cache-dir cache-key-path ensure-cache-parent!
      compile-file-stamp compile-cache-valid? compile-write-cache
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
