(define-library (scheme base)
  (import (goldfish))
  (export let-values
    define-values
    define-record-type
    eqv?
    eq?
    equal?
    =
    <
    >
    <=
    >=
    +
    -
    *
    /
    abs
    square
    exact
    inexact
    max
    min
    floor
    floor/
    ceiling
    truncate
    truncate/
    round
    floor-quotient
    floor-remainder
    gcd
    lcm
    modulo
    quotient
    remainder
    numerator
    denominator
    rationalize
    exact-integer-sqrt
    number->string
    string->number
    number?
    complex?
    real?
    rational?
    integer?
    exact?
    inexact?
    exact-integer?
    positive?
    negative?
    zero?
    odd?
    even?
    not
    boolean=?
    boolean?
    pair?
    cons
    car
    cdr
    set-car!
    set-cdr!
    caar
    cadr
    cdar
    cddr
    null?
    list?
    make-list
    list
    length
    append
    reverse
    list-tail
    list-ref
    list-set!
    memq
    memv
    member
    assq
    assv
    assoc
    list-copy
    map
    for-each
    symbol?
    symbol=?
    string->symbol
    symbol->string
    char?
    char=?
    char<?
    char>?
    char<=?
    char>=?
    char->integer
    integer->char
    string?
    make-string
    string
    string-length
    string-ref
    string-set!
    string-copy
    string-append
    substring
    string-fill!
    string->list
    list->string
    string=?
    string<?
    string>?
    string<=?
    string>=?
    vector?
    make-vector
    vector
    vector-length
    vector-ref
    vector-set!
    vector->list
    list->vector
    vector->string
    string->vector
    vector-copy
    vector-copy!
    vector-fill!
    bytevector?
    make-bytevector
    bytevector
    bytevector-length
    bytevector-u8-ref
    bytevector-u8-set!
    bytevector-copy
    bytevector-append
    utf8->string
    string->utf8
    utf8-string-length
    bytevector-advance-utf8
    call-with-port
    call-with-input-file
    call-with-output-file
    input-port?
    output-port?
    port?
    binary-port?
    textual-port?
    input-port-open?
    output-port-open?
    current-input-port
    current-output-port
    current-error-port
    open-input-file
    open-output-file
    open-binary-input-file
    open-binary-output-file
    close-port
    close-input-port
    close-output-port
    open-input-string
    open-output-string
    get-output-string
    read-char
    peek-char
    read-line
    read-string
    read
    write-char
    newline
    flush-output-port
    eof-object?
    eof-object
    char-ready?
    with-input-from-file
    with-output-to-file
    procedure?
    apply
    string-map
    vector-map
    string-for-each
    vector-for-each
    call-with-current-continuation
    call/cc
    values
    call-with-values
    dynamic-wind
    raise
    guard
    read-error?
    file-error?
  ) ;export
  (begin
    (define-macro (let-values bindings . body)
      (if (null? bindings)
        `(let () ,@body)
        (let ((b (car bindings)) (rest (cdr bindings)))
          `(call-with-values (lambda () ,(cadr b))
             (lambda ,(car b) (let-values ,rest ,@body)))
        ) ;let
      ) ;if
    ) ;define-macro
    (define-macro (define-values vars expression)
      (let* ((tmp (next-fresh "tmp"))
             (setters (let loop
                        ((vs vars) (expr tmp) (acc '()))
                        (if (null? vs)
                          (reverse acc)
                          (loop (cdr vs) (list 'cdr expr) (cons `(set! ,(car vs)
                                                                   (car ,expr)) acc))
                        ) ;if
                      ) ;let
             ) ;setters
            ) ;
        `(begin
           ,@(map (lambda (v) `(define ,v (quote uninitialized))) vars)
           (call-with-values (lambda () ,expression) (lambda ,tmp ,@setters)))
      ) ;let*
    ) ;define-macro
    (define-macro (define-record-type type make ? . fields)
      (let ((rtd (next-record-rtd))
            (make-name (car make))
            (make-params (cdr make))
            (field-names (map car fields))
            (acc-defs (let loop
                        ((fs fields) (i 1))
                        (if (null? fs)
                          '()
                          (let ((acc (cadr (car fs))))
                            (cons `(define (,acc obj) (vector-ref obj ,i))
                              (if (pair? (cddr (car fs)))
                                (let ((mod (caddr (car fs))))
                                  (cons `(define (,mod obj val)
                                           (vector-set! obj ,i val)) (loop (cdr fs) (+ i 1)))
                                ) ;let
                                (loop (cdr fs) (+ i 1))
                              ) ;if
                            ) ;cons
                          ) ;let
                        ) ;if
                      ) ;let
            ) ;acc-defs
           ) ;
        `(begin
           (define ,rtd (make-record-type (quote ,type) (quote ,field-names)))
           ,@(if (keyword? type) '() `((define ,type ,rtd)))
           (define (,make-name ,@make-params) (vector ,rtd ,@make-params))
           (define (,? obj)
             (and (vector? obj)
               (positive? (vector-length obj))
               (eq? (vector-ref obj 0) ,rtd)))
           ,@acc-defs
           (quote ,type))
      ) ;let
    ) ;define-macro
    (define-macro (guard results . body)
      `(let ((caught (catch ,#t
                       (lambda ()
                         (cons (quote normal)
                           (call-with-values (lambda () ,@body) list)))
                       (lambda (type info) (cons (quote raised) (car info))))))
         (if (eq? (car caught) (quote raised))
           (let ((,(car results) (cdr caught)))
             (cond ,@(cdr results) (else (raise ,(car results)))))
           (apply values (cdr caught))))
    ) ;define-macro
  ) ;begin
) ;define-library
