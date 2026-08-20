(set! (*s7* 'scheme-version) 'r7rs)

(define (file-exists? path)
  (unless (string? path)
    (error 'type-error "(file-exists? path): path should be string"))
  (and (g_access path 0) ; exist?
       (or (g_access path 1) ; have permission?
           (error 'permission-error (string-append "No permission: " path)))))

(define (getenv name)
  (let ((e (assoc name (g_getenvs))))
    (and e (cdr e))))

(define (delete-file path)
  (unless (string? path)
    (error 'type-error "(delete-file path): path should be string"))
  (unless (file-exists? path)
    (error 'read-error (string-append path " does not exist")))
  (g_delete-file path))

;;;

(define (fold fn accum list)
  (if (null? list)
    accum
    (fold fn
          (fn accum (car list))
          (cdr list))))

(define (filter pred l)
  (let loop ([l l])
    (if (null? l)
      l
      (let ([head (car l)]
            [tail (cdr l)])
        (if (pred head)
          (let ([new-tail (loop tail)])
            (if (eq? tail new-tail)
              l
              (cons head new-tail)))
          (loop tail))))))

(define (any pred ls)
  (let lp ([ls ls])
    (cond
      ((null? ls)
       #f)
      ((null? (cdr ls))
       (pred (car ls)))
      (else
        (or (pred (car ls)) (lp (cdr ls)))))))

(define (every pred ls)
  (or (null? ls)
      (and (pred (car ls)) (every pred (cdr ls)))))

(define (sort comparator lst)
  (sort! (copy lst) comparator))

;;;

;;; ---------------------------------------------------------------------------
;;; R6RS derived forms the host (s7) does not provide.  The host let-values /
;;; let*-values / define-record-type macros for bootstrap-0 live in
;;; bootstrap-macros.scm (loaded only under GOLDFISH_BOOTSTRAP); the former
;;; host define-library / import macros were removed (the expander handles
;;; r7rs-small purely syntactically).

;;; ---------------------------------------------------------------------------
;;; Expander runtime substrate (seed prelude).  The record runtime, promises,
;;; module substrate, vector-map, fresh-name generation, eof-object and
;;; syntax-error now live in the KERNEL (expander/kernel/substrate.scm), so
;;; the artifact is self-contained; the seed keeps the list utilities (fold /
;;; filter / any / every / sort) and the loader below.  The host
;;; define-record-type macro for bootstrap-0 is in bootstrap-macros.scm.

;;; ---------------------------------------------------------------------------
;;; Loader (seed loader).  Module loader driven by our own R7RS reader
;;; (read-forms).  After this bootstrap file, every source file is read by
;;; the Scheme reader -- never by the interpreter -- and evaluated into the
;;; rootlet.  load-source-file reads a file with the R7RS reader and
;;; evaluates each form into the rootlet.

(define (load-find-module-file path)
  (if (file-exists? path)
    path
    (let loop ((dirs *load-path*))
      (if (null? dirs)
        #f
        (let ((candidate (string-append (car dirs) "/" path)))
          (if (file-exists? candidate)
            candidate
            (loop (cdr dirs))))))))

;;; read-forms : port -> (list datum)
;;; Read all datums from a port (through the Scheme `read'), the loader's
;;; input format.  scsyntax's loader reads whole files this way.
;;; This seed definition runs against the bootstrap reader; reader.scm
;;; rebinds read-forms (and read) to the full R7RS reader once it loads.

(define (read-forms port)
  (let loop ((d (read port)) (acc '()))
    (if (eof-object? d)
      (reverse acc)
      (loop (read port) (cons d acc)))))

(define (load-source-file path)
  (let ((file (load-find-module-file path)))
    (unless file
      (error "load-source-file: file not found" path))
    (let ((port (open-input-file file)))
      (dynamic-wind
        (lambda () #f)
        (lambda () (for-each (lambda (f) (eval f (rootlet)))
                             (read-forms port)))
        (lambda () (close-input-port port))))))

;;; load : path -> void
;;; The bootstrap-0 loader.  Read the whole file, then evaluate each form
;;; THROUGH the expander once it is up (expand-eval, defined by reader.scm),
;;; falling back to plain s7 eval before that (the seed phase).  The R7RS
;;; reader rebinds load to the full caching version once it loads; this seed
;;; version keeps the ONGOING bootstrap-0 load of e.g. build-combined.scm
;;; going through the expander after the reader comes up mid-file (s7's
;;; builtin load would keep s7-evaluating every form, so base-library
;;; bindings such as parse-library-clauses -- visible only to the expander --
;;; would be unbound).
(define (load file)
  (let ((path (load-find-module-file file)))
    (unless path
      (error "load: file not found" file))
    (let ((forms (call-with-input-file path
                   (lambda (p) (read-forms p)))))
      (for-each (lambda (d)
                  (if (defined? 'expand-eval)
                    (expand-eval d)
                    (eval d (rootlet))))
                forms))))

;;; load-expanded : path -> void
;;; Load a plain Scheme file THROUGH the expander.  The source is read with
;;; the bootstrap (tiny) reader, expanded against a fresh library, the
;;; lowered defs are evaluated into the-expander-library (where references
;;; to base-library values resolve), and the file's public entry points are
;;; re-bound in the rootlet (the s7-evaluated bootstrap code resolves them
;;; there).  This is how reader.scm loads: the reader must be available
;;; BEFORE install.scm (the lib-layer files use `(X ...)' ellipsis syntax
;;; that s7's tiny reader collapses), so it loads right after the artifact,
;;; using kernel features only (define-syntax with lambda transformers; the
;;; free kernel identifiers below resolve from the rootlet once the
;;; artifact has loaded).
;;;
(load-source-file "cache/gfo.scm")

;; compat aliases for old le-cache names
(define le-cache-dir gfo-dir)
(define le-cache-key gfo-key)
(define (le-cache-files path) (values (gfo-path path) (gfo-path path)))
(define le-cache-valid? (lambda (cache meta stamp) (gfo-valid? cache stamp)))
(define (le-write-cache cache meta stamp bindings sexp)
  (gfo-write! cache stamp (list bindings sexp)))

(define (le-rootlet-copy bindings)
  (for-each (lambda (e)
              (varlet (rootlet) (car e)
                      (eval (cdr e) the-expander-library)))
            bindings))

(define (load-expanded path . maybe-lib)
  (let ((file (load-find-module-file path)))
    (unless file (error "load-expanded: file not found" path))
    (let* ((lib (if (pair? maybe-lib)
                  (if (eq? (car maybe-lib) 'base)
                    the-base-library
                    (make-exp-library (car maybe-lib)))
                  (make-exp-library '(liii reader))))
           (artifact (or (load-find-module-file "expander/kernel-combined.scm")
                         "expander/kernel-combined.scm"))
           (stamp (list (g_path-getmtime file) (g_path-getsize file)
                        (g_path-getmtime artifact) (g_path-getsize artifact))))
      (let ((gfo-file (gfo-path path)))
        ;; bootstrap-0 (GOLDFISH_BOOTSTRAP): the s7-eval'd kernel is the
        ;; running expander, so the cache (keyed on the COMMITTED artifact's
        ;; stamp) may hold lowering from a different kernel version -- a
        ;; pristine rebuild must re-expand.
        (if (and (not (getenv "GOLDFISH_BOOTSTRAP"))
                 (gfo-valid? gfo-file stamp))
          (let* ((payload (gfo-read gfo-file))
                 (bindings (car payload))
                 (sexp (cadr payload)))
            (eval sexp the-expander-library)
            (le-rootlet-copy bindings))
          (let* ((forms (read-forms (open-input-file file)))
                 (stxs (map (lambda (f) (stx-set-library (wrap-expression f) lib))
                            forms)))
            (call-with-values
              (lambda () (expand-library-body stxs lib (initial-context)))
              (lambda (defs ctx)
              (for-each (lambda (d) (eval (lower d) the-expander-library)) defs)
              (let ((bindings (map (lambda (e)
                                     (let ((name (car e)) (b (cdr e)))
                                       (cons name
                                             (toplevel-ref-gensym
                                               (binding-value b)))))
                                   (filter (lambda (e)
                                             (toplevel-binding? (cdr e)))
                                           (exp-library-bindings lib)))))
                (gfo-write! gfo-file stamp (list bindings (cons 'begin (map lower defs))))
                (le-rootlet-copy bindings))))))))))

;;; ---------------------------------------------------------------------------
;;; Host macros for bootstrap-0 only.  Under GOLDFISH_BOOTSTRAP or a
;;; EXPANDER_BOOT=from-source build, s7 evaluates the kernel sources
;;; directly (no self-hosted expander yet), so the kernel define-record-type
;;; and install.scm's let-values must come from the host.  Normal startup
;;; loads everything through the expander and never touches this file.
(if (or (getenv "GOLDFISH_BOOTSTRAP")
        (and (getenv "EXPANDER_BOOT")
             (string=? (getenv "EXPANDER_BOOT") "from-source")))
  (load-source-file "liii/bootstrap-macros.scm"))
