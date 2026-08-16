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
;;; them through the expander core.

;;; install-library-forms! : exp-library (list datum) -> context
;;; Expand a program body given as ordinary object-level source and install
;;; its definitions into the library.  Macros install at expand time; value
;;; definitions are expanded and their initializers evaluated into
;;; the-expander-library -- the module transformer code is evaluated in -- so
;;; transformer output that references a library value resolves to its
;;; runtime binding.

(define (install-library-forms! lib forms)
  (let ((stxs (map (lambda (form)
                     (stx-set-library (wrap-expression form) lib))
                   forms)))
    (let*-values (((defs ctx)
                   (expand-library-body stxs lib (initial-context))))
      (for-each (lambda (d)
                  (let ((sexp (lower d)))
                    (if (and (pair? sexp) (eq? (car sexp) 'define))
                        (eval sexp the-expander-library)
                        (error "install-library-forms!: expected value definition"
                               sexp))))
                defs)
      ctx)))

;;; install-library-file! : exp-library path -> context
;;; Read a file of object-level R7RS source (with the bundled reader) and
;;; expand it into the library.  File lookup reuses the loader's
;;; load-find-module-file (boot/loader.scm), the single file-finding helper.

(define (install-library-file! lib path)
  (let ((file (load-find-module-file path)))
    (unless file
      (error "install-library-file!: file not found" path))
    (install-library-forms! lib (call-with-input-file file read-forms))))

(define (install-standard-library!)
  (install-library-file! the-base-library "expander/lib/standard.scm"))

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

(define (compile-cache-hot? path stamp)
  (let ((base (string-append (compile-cache-dir) "/" (g_sha256 path))))
    (compile-cache-valid? (string-append base ".scm")
                          (string-append base ".meta")
                          stamp)))

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
