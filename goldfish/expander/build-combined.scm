;;; build-combined.scm
;;; Combine the expander kernel sources into a single pre-expanded artifact --
;;; one library, one expansion -- so the runtime loads one self-contained
;;; core-lambda file whose internal references are already resolved to
;;; gensyms (cf. Racket's src/expander/extract and PopSyntax's combine).
;;;
;;; The expander that performs the combining is obtained one of two ways,
;;; controlled by the EXPANDER_BOOT env var:
;;;
;;;   * from-source (default): bootstrap-0 build.  There is no committed
;;;     artifact yet, so s7 evaluates the kernel sources directly
;;;     (load-source-file expander/kernel/load-kernel.scm); the kernel at
;;;     this stage is ordinary object-level source using only forms s7 can
;;;     evaluate (plus the seed's host-side fallbacks).  driver.scm loads
;;;     the lib layer at its end (expander/lib/install.scm), so the kernel's
;;;     own macros (define-record-type, syntax-case, ...) are available
;;;     during the re-expansion.
;;;
;;;   * from-artifact: bootstrap-N (N>=1) build.  The committed prebuilt
;;;     artifact (kernel-combined.scm) is loaded as the expander, then the
;;;     lib layer on top -- the same chain as the runtime.  The kernel
;;;     source is never evaluated by s7.
;;;
;;; Bootstrap loop (each stage is a git tag bootstrap-N):
;;;   seed + source_N           -> s7-eval kernel -> expand -> artifact_0
;;;   seed + artifact_{N-1}     -> expand (source_N) -> artifact_N
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
  ;; Bootstrap-0: s7 evaluates the kernel sources directly.
  (load-source-file "expander/kernel/load-kernel.scm")
  ;; Bootstrap-N (N>=1): the committed artifact is the expander.
  (begin
    (load-source-file "expander/kernel-combined.scm")
    (load-source-file "expander/lib/install.scm")))
;; Install the standard layer so the kernel's own let-values / let*-values
;; resolve to OUR macros instead of remaining free forms that s7 would
;; natively expand at artifact load time -- the artifact then stays pure
;; core-lambda (bootstrap-6's combine does the same).
(install-standard-library!)

(define output "goldfish/expander/kernel-combined.scm")

;; Host-only forms to strip: anything that only makes sense under s7's
;; native loading (the artifact is loaded through the self-hosted loader,
;; which does not run these).  module-define! registrations are KEPT: the
;; artifact still registers the API into the-expander-library at load time.
(define (host-form? f)
  (and (pair? f) (let ((h (car f)))
                   (or (eq? h 'provide) (eq? h 'require) (eq? h 'autoload)
                       (and (eq? h 'set!)
                            (let ((t (cadr f)))
                              (or (not (symbol? t)) (eq? t '*load-path*))))))))

;; The kernel module list is the same manifest that
;; expander/kernel/load-kernel.scm loads: order matters, dependencies first.
;; driver.scm is excluded here -- the artifact builder appends it separately
;; (driver-forms keeps only its (define ...) forms), matching the way the
;; pre-artifact host path loads the kernel then the driver.
(define (kernel-module-paths)
  (let loop ((fs (call-with-input-file "goldfish/expander/kernel/load-kernel.scm" read-forms))
             (acc '()))
    (if (null? fs)
      (reverse acc)
      (let ((f (car fs)))
        (loop (cdr fs)
              (if (and (pair? f) (eq? (car f) 'load-source-file)
                       (not (string=? (cadr f) "expander/kernel/driver.scm")))
                (cons (string-append "goldfish/" (cadr f)) acc)
                acc))))))

(define modules (kernel-module-paths))

(define (forms-of path)
  (filter (lambda (f) (not (host-form? f)))
          (read-forms (open-input-file path))))

;; Driver entry points (compile-program, expand, ...); keep only the core
;; function definitions, not the lib layer or any boot.  CONTRACT:
;; driver.scm must keep every host-only top-level form a (define ...);
;; anything else is silently dropped here.  The (module-define! ...)
;; registrations at the end of driver.scm are host-only and therefore
;; excluded; the artifact exposes the API via the toplevel re-binding below.
(define (driver-forms)
  (filter (lambda (f) (and (pair? f) (eq? (car f) 'define)))
          (forms-of "goldfish/expander/kernel/driver.scm")))

(define all-forms (append (apply append (map forms-of modules)) (driver-forms)))

;; Expand everything as ONE program against ONE library, so cross-module
;; free references resolve to gensyms directly (no per-module mapping).
(let* ((lib (make-exp-library '(goldfish expander)))
       (stxs (map (lambda (f) (stx-set-library (wrap-expression f) lib)) all-forms)))
  (let*-values (((defs ctx) (expand-library-body stxs lib (initial-context))))
    (let* ((artifact (append (map lower defs)
                             (map (lambda (e) (list 'define (car e)
                                                    (toplevel-ref-gensym (binding-value (cdr e)))))
                                  (filter (lambda (e) (eq? (binding-kind (cdr e)) 'toplevel))
                                          (exp-library-bindings lib)))))
           (artifact (cons 'begin artifact)))
      (let-set! *s7* 'print-length 1000000)
      (call-with-output-file output
        (lambda (port) (write artifact port)))
      (format #t "wrote ~A (~A forms)\n" output (length (cdr artifact))))))
