;;; bootstrap-macros.scm -- host macros for bootstrap-0 only.
;;;
;;; Loaded by the seed (liii/boot.scm) ONLY when GOLDFISH_BOOTSTRAP is set,
;;; i.e. when s7 evaluates the kernel sources directly (bootstrap-0, no
;;; self-hosted expander yet).  The kernel sources use define-record-type and
;;; install.scm (s7-evaluated via load-kernel.scm) uses let-values, so the
;;; host must provide them as define-macro until the expander is running.
;;; Normal startup loads everything through the expander (the lib layer's
;;; syntax-rules versions of these forms), so these host macros are never
;;; exercised there.
;;;
;;; define-library / import are NOT here: the host import path was removed
;;; (the expander handles r7rs-small purely syntactically), so those macros
;;; are dead code and were deleted outright.

;;; let-values : bind to the values of a single producer expression.
;;; The host (s7) lacks this R6RS form, and the lib-layer install code
;;; (install.scm, s7-evaluated at bootstrap-0 before the expander loads)
;;; uses it.
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
