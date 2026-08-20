;; bootstrap-macros.scm: host let-values/define-record-type for bootstrap-0 only (GOLDFISH_BOOTSTRAP).

;; let-values (host lacks R6RS form; install.scm uses it at bootstrap-0).
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

;; let*-values: sequential let-values.
(define-macro (let*-values clauses . body)
  (if (null? clauses)
    `(let () ,@body)
    `(let-values (,(car clauses))
       (let*-values ,(cdr clauses) ,@body))))

;; define-record-type host macro (vector layout; rtd via next-record-rtd for round-trip).
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
