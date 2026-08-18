;;; prelude.scm -- minimal derived forms for the reader's early expansion.
;;;
;;; The R7RS reader (reader.scm) loads through the expander IMMEDIATELY after
;;; the artifact -- before install.scm installs the lib layer.  The lib-layer
;;; files use `(X ...)' ellipsis syntax that s7's tiny reader collapses, so
;;; the R7RS reader must be up first.  But the reader itself uses the standard
;;; derived forms (let / let* / cond / case / when / do / and / or), which the
;;; lib layer normally provides (core-macros.scm / standard.scm) -- a
;;; chicken-and-egg.  This file breaks the cycle: it defines those forms as
;;; define-syntax macros with LAMBDA transformers (only kernel features --
;;; syntax-rules comes with the lib layer), installed into the base library
;;; right after the artifact.  The reader then resolves them like any macro
;;; (hygienic expansion, no host pass-through, no scope-rename gaps).
;;;
;;; The definitions are plain datum->syntax desugarings; identifiers
;;; introduced by a macro (lambda, if, begin, eqv?, memv, ...) resolve in the
;;; macro-use context via the base library / core forms, and re-expansion of
;;; the macro output re-dispatches recursive forms (or / and / let) back to
;;; these very macros.  Later, install.scm's core-macros.scm / standard.scm
;;; install the full syntax-rules versions over the same base-library names,
;;; so user code ends up with the hygienic lib-layer macros.

(define-syntax when
  (lambda (stx)
    (let ((form (syntax->datum stx)))
      (datum->syntax
        stx
        `(if ,(cadr form) (begin ,@(cddr form)) (if #f #f))))))

(define-syntax and
  (lambda (stx)
    (let ((form (syntax->datum stx)))
      (let ((args (cdr form)))
        (datum->syntax
          stx
          (cond ((null? args) #t)
                ((null? (cdr args)) (car args))
                (else `(if ,(car args) (and ,@(cdr args)) #f))))))))

(define-syntax or
  (lambda (stx)
    (let ((form (syntax->datum stx)))
      (let ((args (cdr form)))
        (datum->syntax
          stx
          (cond ((null? args) #f)
                ((null? (cdr args)) (car args))
                (else
                 (let ((t (make-fresh-name 'or-t)))
                   `(let ((,t ,(car args)))
                      (if ,t ,t (or ,@(cdr args))))))))))))

(define-syntax cond
  (lambda (stx)
    (let ((form (syntax->datum stx)))
      (let loop ((clauses (cdr form)))
        (datum->syntax
          stx
          (cond ((null? clauses) '(if #f #f))
                ((and (pair? (car clauses)) (eq? (caar clauses) 'else))
                 `(begin ,@(cdar clauses)))
                ((null? (cdar clauses))
                 `(or ,(caar clauses) ,(loop (cdr clauses))))
                (else
                 `(if ,(caar clauses)
                      (begin ,@(cdar clauses))
                      ,(loop (cdr clauses))))))))))

(define-syntax case
  (lambda (stx)
    (let ((form (syntax->datum stx)))
      (let ((key (cadr form))
            (clauses (cddr form)))
        (let loop ((cls clauses))
          (datum->syntax
            stx
            (cond ((null? cls) '(if #f #f))
                  ((and (pair? (car cls)) (eq? (caar cls) 'else))
                   `(begin ,@(cdar cls)))
                  (else
                   `(if (memv ,key ',(caar cls))
                        (begin ,@(cdar cls))
                        ,(loop (cdr cls)))))))))))

(define-syntax let
  (lambda (stx)
    (let ((form (syntax->datum stx)))
      (let ((first (cadr form))
            (rest (cddr form)))
        (datum->syntax
          stx
          (if (symbol? first)
            ;; Named let: (let name ((v i) ...) body ...)
            (let ((params (map car (car rest)))
                  (inits (map cadr (car rest)))
                  (body (cdr rest)))
              `(letrec ((,first (lambda ,params ,@body)))
                 (,first ,@inits)))
            ;; Value let: (let ((v i) ...) body ...)
            (let ((bindings first)
                  (body rest))
              `((lambda ,(map car bindings) ,@body)
                ,@(map cadr bindings)))))))))

(define-syntax let*
  (lambda (stx)
    (let ((form (syntax->datum stx)))
      (let ((bindings (cadr form))
            (body (cddr form)))
        (let loop ((bs bindings))
          (datum->syntax
            stx
            (if (null? bs)
              `(let () ,@body)
              `(let (,(car bs)) ,(loop (cdr bs))))))))))

(define-syntax do
  (lambda (stx)
    (let ((form (syntax->datum stx)))
      (let ((specs (cadr form))
            (test (caddr form))
            (body (cdddr form)))
        (datum->syntax
          stx
          `(let doloop
               ,(map (lambda (s) (list (car s) (cadr s))) specs)
             (if ,(car test)
               ,(if (pair? (cdr test))
                    `(begin ,@(cdr test))
                    '(if #f #f))
               (begin
                 ,@body
                 (doloop
                   ,@(map (lambda (s)
                            (if (pair? (cddr s)) (caddr s) (car s)))
                          specs))))))))))

;; let-values : bind to the values of a single producer expression
(define-syntax let-values
  (lambda (stx)
    (let ((form (syntax->datum stx)))
      (let ((bindings (cadr form))
            (body (cddr form)))
        (datum->syntax
          stx
          (if (null? bindings)
            `(let () ,@body)
            (let ((b (car bindings)) (rest (cdr bindings)))
              `(call-with-values (lambda () ,(cadr b))
                 (lambda ,(car b) (let-values ,rest ,@body))))))))))

;; let*-values : like let-values but binding clauses are evaluated
;; sequentially (each clause may refer to earlier bindings).
(define-syntax let*-values
  (lambda (stx)
    (let ((form (syntax->datum stx)))
      (let ((bindings (cadr form))
            (body (cddr form)))
        (let loop ((bs bindings))
          (datum->syntax
            stx
            (if (null? bs)
              `(let () ,@body)
              (let ((b (car bs)))
                `(let-values (,(list (car b) (cadr b)))
                   ,(loop (cdr bs)))))))))))
