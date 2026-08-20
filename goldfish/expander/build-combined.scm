;; build-combined.scm: expand goldfish/expander/kernel.scm into kernel-combined.scm.
;; Self-bootstrap: expander re-expands its own source (one lib, one expansion).
;; EXPANDER_BOOT=from-source (bootstrap-0: s7 evals kernel) | from-artifact (N>=1: load artifact).
;; The seed/reader are already loaded by bin/gf; do not (load ...) them here.

;; A program (R7RS 5.1): the environment starts empty, so the internal
;; runtime surface comes from an explicit import of the implementation
;; library (goldfish).
(import (goldfish))

(define (boot-from-source?)
  (let ((v (getenv "EXPANDER_BOOT")))
    (or (not v) (string=? v "from-source"))))

(if (boot-from-source?)
  ;; from-source: s7 evals kernel, then reader, then install (reader must precede install for X ...).
  (begin
    (load-source-file "expander/kernel/load-kernel.scm")
    (load-expanded "liii/prelude.scm" 'base)
    (load-expanded "liii/reader.scm")
    (load-expanded "expander/lib/install.scm" '(expander lib install)))
  ;; from-artifact: artifact+install already loaded by bin/gf; do not reload (would recreate record types).
  (begin))
;; Kernel let-values must resolve to our macros for pure core-lambda artifact.
(install-standard-library!)

(define output "goldfish/expander/kernel-combined.scm")

;; kernel.scm source (include clauses resolve over *load-path*).
(define (kernel-library-form)
  (let ((file "goldfish/expander/kernel.scm"))
    (unless (file-exists? file)
      (error "build-combined: kernel library not found" file))
    (car (read-forms (open-input-file file)))))

;; Drop base PRIMITIVE bindings so value defs win (otherwise bare names fail at load).
(let* ((bl (base-library)))
  (when bl
    (set-exp-library-bindings!
      bl
      (filter (lambda (e) (not (primitive-binding? (cdr e))))
              (exp-library-bindings bl)))))
;; Expand body directly (not expand-define-library: no runtime module registration).
(let* ((form (kernel-library-form))
       (stx (stx-set-library (wrap-expression form) the-base-library))
       (clauses (cddr (syntax-form stx))))
  (let*-values (((exports imports body-stxs) (parse-library-clauses clauses)))
    (let ((lib (make-exp-library '(goldfish))))
      (import-into-library! lib imports)
      (let ((body-stxs (map (lambda (s) (stx-set-library s lib)) body-stxs)))
        (let*-values (((defs ctx) (expand-library-body body-stxs lib (initial-context))))
          (let* ((stray-prims
                   (map car
                        (filter (lambda (e) (primitive-binding? (cdr e)))
                                (exp-library-bindings lib))))
                  ;; Re-bind all toplevels to rootlet: lib refs as free ids + internal accessors.
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
