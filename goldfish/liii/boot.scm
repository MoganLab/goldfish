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

(define-macro (define-library libname . body)
  `(define ,(symbol (object->string libname))
     (with-let (sublet (unlet)
                 (cons 'import import)
                 (cons '*export* ())
                 (cons 'export
                   (define-macro (,(next-fresh "export-macro") . names)
                     (list 'set!
                           '*export*
                           (list 'append
                                 (list 'quote names)
                                 '*export*)))))
       ,@body
       (apply inlet
         (map (lambda (entry)
                (if (or (member (car entry) '(*export* export import))
                      (and (pair? *export*) (not (member (car entry) *export*))))
                  (values)
                  entry))
           (curlet))))))

(unless (defined? 'r7rs-import-library-filename)
  (define (r7rs-import-library-filename libs)
    (when (pair? libs)
      (let ((lib (if (memq (caar libs) '(only except prefix rename)) (cadar libs) (car libs))))
        (when (not (defined? (symbol (object->string lib))))
          (load (let loop
                  ((parts lib) (name ""))
                  (set! name (string-append name (symbol->string (car parts))))
                  (if (null? (cdr parts))
                    (string-append name ".scm")
                    (begin
                      (set! name (string-append name "/"))
                      (loop (cdr parts) name))))))
        (r7rs-import-library-filename (cdr libs))))))

(define-macro (import . libs)
  `(begin
     (r7rs-import-library-filename (quote ,libs))
     (varlet (curlet)
       ,@(map (lambda (lib)
                (case (car lib)
                      ((only)
                       `((lambda (e names)
                           (apply inlet
                             (map (lambda (name) (cons name (e name))) names)))
                         (symbol->value (symbol (object->string (cadr (quote ,lib)))))
                         (cddr (quote ,lib))))
                      ((except)
                       `((lambda (e names)
                           (apply inlet
                             (map (lambda (entry)
                                    (if (member (car entry) names)
                                      (values)
                                      entry))
                               e)))
                         (symbol->value (symbol (object->string (cadr (quote ,lib)))))
                         (cddr (quote ,lib))))
                      ((prefix)
                       `((lambda (e prefx)
                           (apply inlet
                             (map (lambda (entry)
                                    (cons
                                      (string->symbol (string-append
                                                        (symbol->string prefx)
                                                        (symbol->string (car entry))))
                                      (cdr entry)))
                               e)))
                         (symbol->value (symbol (object->string (cadr (quote ,lib)))))
                         (caddr (quote ,lib))))
                      ((rename)
                       `((lambda (e names)
                           (apply inlet
                             (map (lambda (entry)
                                    (let ((info (assoc (car entry) names)))
                                      (if info
                                        (cons (cadr info) (cdr entry))
                                        entry)))
                               e)))
                         (symbol->value (symbol (object->string (cadr (quote ,lib)))))
                         (cddr (quote ,lib))))
                      (else `(let ((sym (symbol (object->string (quote ,lib)))))
                               (if (not (defined? sym))
                                 (format () "~A not loaded~%" sym)
                                 (symbol->value sym))))))
            libs))))

;;; ---------------------------------------------------------------------------
;;; R6RS derived forms the host (s7) does not provide.  The expander kernel
;;; sources (goldfish/expander/kernel) use these freely; at bootstrap-0 they
;;; are evaluated directly by s7, so the seed provides host-side fallbacks.
;;; Once the expander is self-hosted these become OUR macros (lib layer), and
;;; the host forms below are only exercised during the bootstrap.

;;; let-values : bind to the values of a single producer expression.
;;; The host (s7) lacks this R6RS form, and the lib-layer install code
;;; (install.scm / module.scm, s7-evaluated before the expander loads) uses
;;; it -- that was the sole reason (scheme base) had to be host-imported
;;; ahead of the expander.  Providing it in the seed lets r7rs-small load
;;; entirely through the expander (pure syntax, no host varlet import).
(define-macro (let-values bindings . body)
  (if (null? bindings)
    `(let () ,@body)
    (let ((b (car bindings)) (rest (cdr bindings)))
      `(call-with-values
         (lambda () ,(cadr b))
         (lambda ,(car b)
           (let-values ,rest ,@body))))
  ) ;if
) ;define-macro

;;; let*-values : like let-values but binding clauses are evaluated
;;; sequentially (each clause may refer to earlier bindings).  Expand to
;;; nested let-values.
(define-macro (let*-values clauses . body)
  (if (null? clauses)
    `(let () ,@body)
    `(let-values (,(car clauses))
       (let*-values ,(cdr clauses) ,@body))))

;;; ---------------------------------------------------------------------------
;;; Expander runtime substrate (seed prelude).  The record runtime, promises,
;;; module substrate, vector-map, fresh-name generation, eof-object and
;;; syntax-error now live in the KERNEL (expander/kernel/substrate.scm), so
;;; the artifact is self-contained; the seed keeps the list utilities (fold /
;;; filter / any / every / sort) and the loader below.

;;; define-record-type as a host macro (bootstrap-0): expands to code that
;;; builds a descriptor plus vector-layout constructor / predicate /
;;; accessors / modifiers, using the kernel's record runtime (make-record-type
;;; et al., expander/kernel/substrate.scm).  The constructor takes one
;;; argument per field, in declaration order (all kernel and library uses are
;;; of this shape).  The rtd name is a fresh READABLE symbol (counter-based,
;;; not s7's {gensym}-N:M): the expander artifact is written as Scheme source
;;; and must round-trip through the R7RS reader.

(define-macro (define-record-type type make ? . fields)
  (let ((rtd (next-record-rtd))
        (make-name (car make))
        (make-params (cdr make))
        (field-names (map car fields))
        (acc-defs
          (let loop ((fs fields) (i 1))
            (if (null? fs)
              '()
              (let ((acc (cadr (car fs))))
                (cons `(define (,acc obj) (vector-ref obj ,i))
                      (if (pair? (cddr (car fs)))
                        (let ((mod (caddr (car fs))))
                          (cons `(define (,mod obj val) (vector-set! obj ,i val))
                                (loop (cdr fs) (+ i 1))))
                        (loop (cdr fs) (+ i 1)))))))))
    `(begin
       (define ,rtd (make-record-type ',type ',field-names))
       (define (,make-name ,@make-params) (vector ,rtd ,@make-params))
       (define (,? obj)
         (and (vector? obj)
              (positive? (vector-length obj))
              (eq? (vector-ref obj 0) ,rtd)))
       ,@acc-defs)))

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
;;; The lowered defs are deterministic, so they are cached (like the
;;; lib-layer ccache): re-expanding the reader through the expander costs
;;; ~250ms per warm start, and warm starts should only pay the cheap
;;; eval+rootlet-copy.  The cache key is the source path; validity is the
;;; source's mtime+size AND the artifact's (the kernel version determines
;;; the lowering), so rebuilding the artifact invalidates it.  Format (one
;;; datum per line, written with s7's write + raised print-length, read by
;;; the bootstrap tiny reader):
;;;   (le-cache 1)
;;;   ((name . gensym) ...)       ; toplevel value bindings for the rootlet
;;;   (begin <lowered def> ...)   ; eval'd in the-expander-library

(define (le-cache-dir)
  (let ((xdg (getenv "XDG_CACHE_HOME")))
    (string-append
      (if (and xdg (not (string=? xdg ""))) xdg
        (string-append (or (getenv "HOME") "/tmp") "/.cache"))
      "/goldfish/ccache")))

(define (le-cache-key path)
  (let ((chars (string->list path)))
    (list->string
      (map (lambda (c)
             (cond ((char=? c #\/) #\_)
                   ((char=? c #\.) #\_)
                   (else c)))
           chars))))

(define (le-cache-files path)
  (let ((dir (le-cache-dir)))
    (values (string-append dir "/" (le-cache-key path) ".le")
            (string-append dir "/" (le-cache-key path) ".le.meta"))))

(define (le-cache-valid? cache meta stamp)
  (and (file-exists? cache)
       (file-exists? meta)
       (let ((rec (call-with-input-file meta
                     (lambda (p) (car (read-forms p))))))
         (and (pair? rec)
              (pair? (car rec))
              (equal? (car rec) '(le-cache 1))
              (equal? (cdr rec) stamp)))))

(define (le-write-cache cache meta stamp bindings sexp)
  (if (getenv "GOLDFISH_CACHE_READONLY")
    #f
    (let ((dir (le-cache-dir)))
      (if (not (file-exists? dir)) (g_mkdir dir))
      (let ((old-length (*s7* 'print-length)))
        (let-set! *s7* 'print-length 1000000)
        (let ((tmp (string-append cache ".tmp")))
          (call-with-output-file tmp
            (lambda (p)
              (write '(le-cache 1) p) (newline p)
              (write bindings p) (newline p)
              (write sexp p)))
          (g_rename tmp cache))
        (let ((mtmp (string-append meta ".tmp")))
          (call-with-output-file mtmp
            (lambda (p) (write (cons '(le-cache 1) stamp) p)))
          (g_rename mtmp meta))
        (let-set! *s7* 'print-length old-length)))))

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
      (let*-values (((cache meta) (le-cache-files path)))
        (if (le-cache-valid? cache meta stamp)
          (let ((rec (call-with-input-file cache
                        (lambda (p) (read-forms p)))))
            (eval (caddr rec) the-expander-library)
            (le-rootlet-copy (cadr rec)))
          (let* ((forms (read-forms (open-input-file file)))
                 (stxs (map (lambda (f) (stx-set-library (wrap-expression f) lib))
                            forms)))
            (let*-values (((defs ctx) (expand-library-body stxs lib (initial-context))))
              (for-each (lambda (d) (eval (lower d) the-expander-library)) defs)
              (let ((bindings (map (lambda (e)
                                     (let ((name (car e)) (b (cdr e)))
                                       (cons name
                                             (toplevel-ref-gensym
                                               (binding-value b)))))
                                   (filter (lambda (e)
                                             (toplevel-binding? (cdr e)))
                                           (exp-library-bindings lib)))))
                (le-write-cache cache meta stamp bindings
                                (cons 'begin (map lower defs)))
                (le-rootlet-copy bindings)))))))))
