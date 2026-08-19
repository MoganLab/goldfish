;;; driver.scm
;;; The EXPANDER CORE entry points:
;;;   expand-stx      expr  -> fully-expanded syntax object
;;;   expand          expr  -> lowered core Scheme S-expression
;;;   compile-program exprs -> lowered (begin <lib-defs>... <body>)
;;;   compile-file    path  -> read a file (R7RS reader) and expand it
;;;
;;; Expansion produces syntax throughout (bindings resolved to allocated
;;; names, every node a syntax object); `lower' (expand.scm) strips
;;; contexts at this boundary to yield evaluable core Scheme.
;;;
;;; This is the expander core only.  The user-space macro library (lib/) is
;;; a separate layer built on top of this core API (see lib/install.scm);
;;; it is NOT part of the expander core / the pre-expanded artifact.  The
;;; bootstrap-0 host path loads it after this file via load-kernel.scm; the
;;; library path (goldfish/expander/kernel.scm) and the runtime load
;;; lib/install.scm separately after the artifact.
;;;
;;; The trailing module-define! registrations expose the driver entry points
;;; through the-expander-library; they are part of both the library body and
;;; the bootstrap-0 host load (harmless duplication -- the artifact re-binds
;;; the same names into the rootlet, so both resolve to the same values).

(define (wrap-expression expr)
  (datum->syntax (make-syntax 'empty (stx-ctx-empty) the-base-library) expr))

(define (expand-stx expr)
  (let-values (((stx ctx)
                (expand-expr (wrap-expression expr) (initial-context))))
    stx))

(define (expand expr)
  (lower (expand-stx expr)))

(define (compile-program exprs)

  (let-values (((program ctx)
                (compile-program* exprs (initial-context))))
    (lower program)))

;;; compile-program-into : (list datum) exp-library -> lowered core
;;; Compile a top-level program into a specific library (R7RS 5.1 program
;;; semantics): the program's environment starts empty (core forms +
;;; module forms + whatever its imports provide) and free identifiers that
;;; resolve nowhere are errors, not ambient base-library names.

(define (compile-program-into exprs lib)
  (let-values (((program ctx)
                (compile-program* exprs (initial-context) lib)))
    (lower program)))

;;; compile-toplevel : datum -> lowered core Scheme S-expression
;;; Compile a single top-level form into the-base-library, so a top-level
;;; define-syntax registers the macro in the shared base library and later
;;; compile-program calls can resolve it.  Value definitions are lowered to
;;; a single (define ...) form; macro definitions produce no runtime form
;;; (the transformer lives only at expand time) and compile to a no-op.
;;; This is the per-form entry point used by the REPL / load / eval path,
;;; keeping macro state across separate eval calls.

(define (compile-toplevel expr)

  (let ((lib the-base-library))
    (let ((stx (stx-set-library (wrap-expression expr) lib)))
      (let*-values (((defs ctx)
                     (expand-library-body (list stx) lib (initial-context))))
        (if (null? defs)
          '(if #f #f)
          (lower (car defs)))))))

;;; compile-file : string -> lowered core Scheme S-expression
;;; Read a source file with the R7RS reader and expand it as a program.

(define (compile-file path)
  (compile-program (call-with-input-file path read-forms)))

(define (compile-file-into path lib)
  (compile-program-into (call-with-input-file path read-forms) lib))

(define (compile-program* exprs ctx . maybe-lib)
  (let ((lib (if (pair? maybe-lib) (car maybe-lib) the-base-library)))
  (let loop ((exprs    exprs)
             (ctx      ctx)
             (lib-defs '())
             (body     '())
             (n        0))
    (when (> n 50000)
      (error "compile-program*: expansion limit exceeded"))
    (if (null? exprs)
      (values (if (null? body)
                (if (null? lib-defs)
                  (wrap-expression '(if #f #f))
                  (wrap-expression (cons 'begin (reverse lib-defs))))
                (wrap-expression
                  (cons 'begin (append (reverse lib-defs) (reverse body)))))
              ctx)
      (let ((expr (car exprs)))
        (if (and (pair? expr) (eq? (car expr) 'begin))
          (loop (append (cdr expr) (cdr exprs)) ctx lib-defs body (+ n 1))
          (let* ((stx  (stx-set-library (wrap-expression expr) lib))
                 (form (syntax-form stx))
                 (head (and (pair? form) (car form))))
            (if (identifier? head)
              (let*-values (((name binding) (resolve-identifier head ctx)))
                (cond
                  ((module-form-binding? binding)
                   (let*-values (((defs ctx1) ((binding-value binding) stx ctx)))
                     (loop (cdr exprs) ctx1 (append (reverse defs) lib-defs) body (+ n 1))))
                  ((eq? name 'eval-when)
                   ;; R7RS eval-when: the expand situation runs NOW (so its
                   ;; effects, e.g. (set! *load-path* ...), are visible to
                   ;; subsequent imports / expansion); load/eval situations
                   ;; are deferred to the body like other expressions.
                   (let* ((wform (syntax-form stx))
                          (sit-datum (map syntax->datum (syntax-form (cadr wform))))
                          (wbody (cddr wform)))
                     (check-eval-when-situations sit-datum stx)
                     (let*-values (((ctx1)
                                    (if (memq 'expand sit-datum)
                                      (eval-when-expand! wbody ctx)
                                      (values ctx))))
                       (if (or (memq 'load sit-datum) (memq 'eval sit-datum))
                         (loop (append wbody (cdr exprs)) ctx1
                               lib-defs body (+ n 1))
                         (loop (cdr exprs) ctx1 lib-defs body (+ n 1))))))
                  (else
                   ;; Expand each top-level form in order (R7RS 5.1 program
                   ;; semantics): a definition is bound immediately, so a
                   ;; later redefinition does not retroactively capture
                   ;; earlier references (e.g. (define x 1) (define y x)
                   ;; (define x 2) must bind y to 1).  The library-body
                   ;; hoisting used for define-library bodies would resolve
                   ;; y against the final x.
                   (let*-values (((d ctx1)
                                  (expand-library-body (list stx) lib ctx)))
                     (loop (cdr exprs) ctx1 lib-defs
                           (append (reverse d) body) (+ n 1))))))
              (let*-values (((d ctx1)
                             (expand-library-body (list stx) lib ctx)))
                (loop (cdr exprs) ctx1 lib-defs
                      (append (reverse d) body) (+ n 1)))))))))))

(module-define! the-expander-library 'expand-stx expand-stx)
(module-define! the-expander-library 'expand expand)
(module-define! the-expander-library 'compile-program compile-program)
(module-define! the-expander-library 'compile-program-into compile-program-into)
(module-define! the-expander-library 'compile-toplevel compile-toplevel)
(module-define! the-expander-library 'compile-file compile-file)
(module-define! the-expander-library 'compile-file-into compile-file-into)

