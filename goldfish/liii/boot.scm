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

;; Seed list utils + loader. Substrate (records/promises/module/vector-map)
;; lives in kernel/substrate.scm so the artifact is self-contained.
;; bootstrap-macros.scm provides host define-record-type/let-values for
;; bootstrap-0 (GOLDFISH_BOOTSTRAP / EXPANDER_BOOT=from-source).

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

;; read-forms: read all datums (seed uses tiny reader; reader.scm rebinds to R7RS).

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

;; bootstrap-0 load: through expand-eval when up, else s7 eval. R7RS reader rebinds later.
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

;; load-expanded: expand with tiny reader against the expander, eval into
;; the-expander-library, copy bindings to rootlet. reader.scm uses this.
(load-source-file "core/gfo.scm")

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
        ;; bootstrap-0: s7 kernel may differ from committed artifact; force re-expand.
        (let ((payload (and (not (getenv "GOLDFISH_BOOTSTRAP"))
                            (gfo-load gfo-file stamp))))
          (if payload
            (let ((bindings (car payload))
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
                (le-rootlet-copy bindings)))))))))))

;; bootstrap-0 only: s7 evaluates kernel sources directly.
(if (or (getenv "GOLDFISH_BOOTSTRAP")
        (and (getenv "EXPANDER_BOOT")
             (string=? (getenv "EXPANDER_BOOT") "from-source")))
  (load-source-file "liii/bootstrap-macros.scm"))
