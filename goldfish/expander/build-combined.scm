;;; build-combined.scm
;;; Combine the expander kernel into a single pre-expanded artifact --
;;; one library, one expansion -- so the runtime loads one self-contained
;;; core-lambda file whose internal references are already resolved to
;;; gensyms (cf. Racket's src/expander/extract and PopSyntax's combine).
;;;
;;; The kernel source IS a library: goldfish/expander/kernel.scm
;;; (define-library (goldfish expander)) whose body includes the kernel
;;; files.  The artifact build expands that library with the running
;;; expander (expand-define-library), so the build is the SELF-BOOTSTRAP:
;;; the expander re-expands its own source to produce the next artifact.
;;;
;;; The expander performing the build is obtained one of two ways,
;;; controlled by the EXPANDER_BOOT env var:
;;;
;;;   * from-source (default): bootstrap-0 build.  There is no committed
;;;     artifact yet, so s7 evaluates the kernel sources directly
;;;     (load-source-file expander/kernel/load-kernel.scm, which also loads
;;;     the lib layer); the running expander is the s7-evaluated kernel.
;;;
;;;   * from-artifact: bootstrap-N (N>=1) build.  The committed prebuilt
;;;     artifact (kernel-combined.scm) is loaded as the expander, then the
;;;     lib layer on top -- the same chain as the runtime.  The kernel
;;;     source is never evaluated by s7.
;;;
;;; Bootstrap loop (each stage is a git tag bootstrap-N):
;;;   seed + source_N           -> s7-eval kernel -> expand (kernel.scm) -> artifact_0
;;;   seed + artifact_{N-1}     -> expand (kernel.scm) -> artifact_N
;;;
;;; Usage: EXPANDER_BOOT=from-source|from-artifact bin/gf build-combined.scm
;;; Output: goldfish/expander/kernel-combined.scm
;;; Note: the seed (liii/boot.scm) and the Scheme reader (liii/reader.scm)
;;; are loaded by bin/gf's bootstrap at startup; do not (load ...) them
;;; here -- load now goes through the expander, which cannot process the
;;; seed's host-specific forms (e.g. let-set!).

(define (boot-from-source?)
  (let ((v (getenv "EXPANDER_BOOT")))
    (or (not v) (string=? v "from-source"))))

(if (boot-from-source?)
  ;; Bootstrap-0: s7 evaluates the kernel sources directly (load-kernel.scm),
  ;; then the R7RS reader comes up THROUGH that kernel (load-expanded, the
  ;; runtime chain minus the artifact), then install.scm -- whose top-level
  ;; calls install the lib layer, whose files use `(X ...)' ellipsis that the
  ;; tiny reader collapses -- so the reader must precede it.
  (begin
    (load-source-file "expander/kernel/load-kernel.scm")
    (load-expanded "liii/prelude.scm" 'base)
    (load-expanded "liii/reader.scm")
    (load-expanded "expander/lib/install.scm" '(expander lib install)))
  ;; Bootstrap-N (N>=1): the committed artifact is the expander.
  (begin
    (load-source-file "expander/kernel-combined.scm")
    (load-expanded "expander/lib/install.scm" '(expander lib install))))
;; Install the standard layer so the kernel's own let-values / let*-values
;; resolve to OUR macros instead of remaining free forms that s7 would
;; natively expand at artifact load time -- the artifact then stays pure
;; core-lambda (bootstrap-6's combine does the same).
(install-standard-library!)

(define output "goldfish/expander/kernel-combined.scm")

;; The kernel library source: goldfish/expander/kernel.scm, read from the
;; repo root (build-combined runs there).  Its include clauses resolve over
;; *load-path* (the goldfish library root).
(define (kernel-library-form)
  (let ((file "goldfish/expander/kernel.scm"))
    (unless (file-exists? file)
      (error "build-combined: kernel library not found" file))
    (car (read-forms (open-input-file file)))))

;; Expand the kernel library.  The base library's PRIMITIVE bindings are
;; dropped first: when a name is both a primitive (registered via
;; install-primitives!) and a value definition (e.g. make-syntax from
;; define-record-type), the primitive binding would win resolution and free
;; references would emit the bare name -- which does not resolve at artifact
;; load time (the artifact is evaluated in the rootlet, and expander
;; primitives live only as bindings).  With the primitives removed, those
;; references resolve to the value definition's gensym, which the artifact
;; defines and loads cleanly.
(let* ((bl (base-library)))
  (when bl
    (set-exp-library-bindings!
      bl
      (filter (lambda (e) (not (primitive-binding? (cdr e))))
              (exp-library-bindings bl)))))
;; Expand the library body directly (NOT via expand-define-library): that
;; entry also emits a runtime module registration expression, but the kernel
;; IS the base library -- it has no runtime module registration to perform
;; (the artifact installs it into the rootlet / the-expander-library via the
;; re-bindings and the driver's module-define! forms; module.scm's
;; runtime-registered-add! is not even loaded yet when the artifact runs).
;; parse-library-clauses handles the define-library surface (including R7RS
;; include splicing, which is how kernel.scm's body pulls in the kernel
;; files).
(let* ((form (kernel-library-form))
       (stx (stx-set-library (wrap-expression form) the-base-library))
       (clauses (cddr (syntax-form stx))))
  (let*-values (((exports imports body-stxs) (parse-library-clauses clauses)))
    (let ((lib (make-exp-library '(goldfish expander))))
      (import-into-library! lib imports)
      (let ((body-stxs (map (lambda (s) (stx-set-library s lib)) body-stxs)))
        (let*-values (((defs ctx) (expand-library-body body-stxs lib (initial-context))))
          (let* ((stray-prims
                   (map car
                        (filter (lambda (e) (primitive-binding? (cdr e)))
                                (exp-library-bindings lib))))
                 ;; Re-bind EVERY top-level binding into the rootlet (runtime
                 ;; semantics): the lib layer references the core API as
                 ;; rootlet free identifiers, and macro expansion generates
                 ;; internal accessors (record selectors etc.) beyond the
                 ;; curated export list -- they must stay reachable.  The
                 ;; export list itself is the public API surface for
                 ;; importers of (goldfish expander); it is verified only for
                 ;; typos (stray-prims above), not for completeness against
                 ;; the generated bindings.
                 (re-bindings
                   (map (lambda (e)
                          (list 'define (car e)
                                (toplevel-ref-gensym (binding-value (cdr e)))))
                        (filter (lambda (e) (eq? (binding-kind (cdr e)) 'toplevel))
                                (exp-library-bindings lib)))))
            (when (pair? stray-prims)
              (error "build-combined: kernel exports reference unexpected primitives"
                     stray-prims))
            (let* ((artifact (append (map lower defs) re-bindings))
                   (artifact (cons 'begin artifact)))
              (let-set! *s7* 'print-length 1000000)
              (call-with-output-file output
                (lambda (port) (write artifact port)))
              (format #t "wrote ~A (~A forms)\n" output (length (cdr artifact))))))))))
