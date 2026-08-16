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
;;; it is loaded at the end of this file as a host convenience, but it is
;;; NOT part of the pre-expanded core artifact produced by blue combine.
;;;
;;; BUILD CONTRACT with build-aux/combine.scm: the artifact builder keeps
;;; ONLY the top-level (define ...) forms of this file (combine.scm's
;;; driver-forms).  Keep every host-only top-level form here a define, or
;;; it is silently dropped from build/kernel-combined.scm.

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

(define (compile-program* exprs ctx)
  (let loop ((exprs    exprs)
             (ctx      ctx)
             (lib-defs '())
             (body     '())
             (n        0))
    (when (> n 50000)
      (error "compile-program*: expansion limit exceeded"))
    (if (null? exprs)
      (if (null? body)
        (values (if (null? lib-defs)
                  (wrap-expression '(if #f #f))
                  (wrap-expression (cons 'begin (reverse lib-defs))))
                ctx)
        (let*-values (((body-defs ctx1)
                       (expand-library-body (reverse body) the-base-library ctx)))
          (let ((body-stx (wrap-expression
                           (cons 'begin (map lower body-defs)))))
            (values (if (null? lib-defs)
                      body-stx
                      (wrap-expression
                        (cons 'begin (append (reverse lib-defs) (list body-stx)))))
                    ctx1))))
      (let ((expr (car exprs)))
        (if (and (pair? expr) (eq? (car expr) 'begin))
          (loop (append (cdr expr) (cdr exprs)) ctx lib-defs body (+ n 1))
          (let* ((stx  (wrap-expression expr))
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
                   (loop (cdr exprs) ctx lib-defs (cons stx body) (+ n 1)))))
              (loop (cdr exprs) ctx lib-defs (cons stx body) (+ n 1)))))))))

(module-define! the-expander-library 'expand-stx expand-stx)
(module-define! the-expander-library 'expand expand)
(module-define! the-expander-library 'compile-program compile-program)
(module-define! the-expander-library 'compile-toplevel compile-toplevel)
(module-define! the-expander-library 'compile-file compile-file)

;;; Host convenience: load the lib layer (the user-space macro library) on
;;; top of the core.  The pre-expanded artifact excludes this; the runtime
;;; loads lib/install.scm separately after the artifact.  Always reached
;;; through the self-hosted loader (the seed has run), so no s7 load here.
(load-source-file "expander/lib/install.scm")

