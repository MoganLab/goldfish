(define-library (scheme base)
  (import (goldfish) (rename (goldfish) (error host-error)))
  (export
    ;; ------------------------------------------------------------------
    ;; R7RS (scheme base) 导出清单
    ;; 按 stdmod.tex 的顺序排列；尚未实现的条目以注释标记其位置。
    ;; ------------------------------------------------------------------
    *
    +
    -
    ...
    /
    <
    <=
    =
    =>
    >
    >=
    _
    abs
    and
    append
    apply
    assoc
    assq
    assv
    begin
    binary-port?
    boolean=?
    boolean?
    bytevector
    bytevector-append
    bytevector-copy
    bytevector-copy!
    bytevector-length
    bytevector-u8-ref
    bytevector-u8-set!
    bytevector?
    caar
    cadr
    call-with-current-continuation
    call-with-port
    call-with-values
    call/cc
    car
    case
    cdar
    cddr
    cdr
    ceiling
    char->integer
    char-ready?
    char<=?
    char<?
    char=?
    char>=?
    char>?
    char?
    close-input-port
    close-output-port
    close-port
    complex?
    cond
    cond-expand
    cons
    current-error-port
    current-input-port
    current-output-port
    define
    define-record-type
    define-syntax
    define-values
    denominator
    do
    dynamic-wind
    else
    eof-object
    eof-object?
    eq?
    equal?
    eqv?
    error
    error-object?
    error-object-irritants
    error-object-message
    even?
    exact
    exact-integer-sqrt
    exact-integer?
    exact?
    expt
    features
    file-error?
    floor
    floor-quotient
    floor-remainder
    floor/
    flush-output-port
    for-each
    gcd
    get-output-bytevector
    get-output-string
    guard
    if
    include
    include-ci
    inexact
    inexact?
    input-port-open?
    input-port?
    integer->char
    integer?
    lambda
    lcm
    length
    let
    let*
    let*-values
    let-syntax
    let-values
    letrec
    letrec*
    letrec-syntax
    list
    list->string
    list->vector
    list-copy
    list-ref
    list-set!
    list-tail
    list?
    make-bytevector
    make-list
    make-parameter
    make-string
    make-vector
    map
    max
    member
    memq
    memv
    min
    modulo
    negative?
    newline
    not
    null?
    number->string
    number?
    numerator
    odd?
    open-input-bytevector
    open-input-string
    open-output-bytevector
    open-output-string
    or
    output-port-open?
    output-port?
    pair?
    parameterize
    peek-char
    peek-u8
    port?
    positive?
    procedure?
    quasiquote
    quote
    quotient
    raise
    raise-continuable
    rational?
    rationalize
    read-bytevector
    read-bytevector!
    read-char
    read-error?
    read-line
    read-string
    read-u8
    real?
    remainder
    reverse
    round
    set!
    set-car!
    set-cdr!
    square
    string
    string->list
    string->number
    string->symbol
    string->utf8
    string->vector
    string-append
    string-copy
    string-copy!
    string-fill!
    string-for-each
    string-length
    string-map
    string-ref
    string-set!
    string<=?
    string<?
    string=?
    string>=?
    string>?
    string?
    substring
    symbol->string
    symbol=?
    symbol?
    syntax-error
    syntax-rules
    textual-port?
    truncate
    truncate-quotient
    truncate-remainder
    truncate/
    u8-ready?
    unless
    unquote
    unquote-splicing
    utf8->string
    values
    vector
    vector->list
    vector->string
    vector-append
    vector-copy
    vector-copy!
    vector-fill!
    vector-for-each
    vector-length
    vector-map
    vector-ref
    vector-set!
    vector?
    when
    with-exception-handler
    write-bytevector
    write-char
    write-string
    write-u8
    zero?
    ;; ------------------------------------------------------------------
    ;; Goldfish 扩展（非 R7RS 规范导出，为兼容现有代码保留）
    ;; ------------------------------------------------------------------
    read
    write
    display
    call-with-input-file
    call-with-output-file
    open-input-file
    open-output-file
    open-binary-input-file
    open-binary-output-file
    with-input-from-file
    with-output-to-file
    delay
    delay-force
    utf8-string-length
    bytevector-advance-utf8
    catch
    throw
  ) ;export
  (begin
    (define (list-set! ls k obj)
      (unless (pair? ls)
        (host-error 'wrong-type-arg "list-set!: expected a non-empty list" ls))
      (unless (and (integer? k) (exact? k) (>= k 0))
        (host-error 'out-of-range "list-set!: invalid index" k))
      (let loop ((tail ls) (index k))
        (cond
          ((null? tail)
           (host-error 'out-of-range "list-set!: index out of range" k))
          ((not (pair? tail))
           (host-error 'wrong-type-arg "list-set!: expected a proper list" ls))
          ((zero? index) (set-car! tail obj))
          (else (loop (cdr tail) (- index 1))))))

    (define (list? x) (proper-list? x))
    (define (length x)
      (cond [(null? x) 0]
            [(pair? x)
             (if (proper-list? x)
                 (let loop ((lst x) (n 0))
                   (if (null? lst) n (loop (cdr lst) (+ n 1))))
                 (error 'wrong-type-arg "length: not a proper list" x))]
            [(string? x) (string-length x)]
            [(vector? x) (vector-length x)]
            [(bytevector? x) (bytevector-length x)]
            [else #f]))

    ;; R7RS 辅助语法关键字（auxiliary syntax）：核心展开器按名字识别它们，
    ;; 这里绑定为空语法，使 (scheme base) 可以导出这些标识符。
    (define-syntax ...
      (syntax-rules ()
      ) ;syntax-rules
    ) ;define-syntax
    (define-syntax _
      (syntax-rules ()
      ) ;syntax-rules
    ) ;define-syntax
    (define-syntax =>
      (syntax-rules ()
      ) ;syntax-rules
    ) ;define-syntax
    (define-syntax else
      (syntax-rules ()
      ) ;syntax-rules
    ) ;define-syntax
    (define-syntax unquote
      (syntax-rules ()
      ) ;syntax-rules
    ) ;define-syntax
    (define-syntax unquote-splicing
      (syntax-rules ()
      ) ;syntax-rules
    ) ;define-syntax

    ;; R7RS make-parameter
    (define* (make-parameter init (converter #f))
      (let* ((convert (or converter (lambda (x) x))) (value (convert init)))
        (lambda args
          (cond
            ((null? args) value)
            ((null? (cdr args)) (set! value (convert (car args))))
            ((and (null? (cddr args)) (eq? (cadr args) '%parameter-convert))
             (convert (car args)))
            ((and (null? (cddr args)) (eq? (cadr args) '%parameter-exchange))
             (let ((old value)) (set! value (car args)) old))
            (else (error "invalid parameter arguments"))))
      ) ;let*
    ) ;define*

    ;; R7RS features：返回当前可用的特性列表
    (define (features)
      *features*
    ) ;define

    (define (with-exception-handler handler thunk)
      (%native-with-exception-handler handler thunk)
    ) ;define

    (define (raise-continuable obj)
      (%native-raise-continuable obj)
    ) ;define

    ;; R7RS u8-ready?：goldfish 不区分文本/二进制端口，
    ;; 以 char-ready? 实现。
    (define (u8-ready? . maybe-port)
      (apply char-ready? maybe-port)
    ) ;define

    ;; R7RS read-bytevector k [port]：
    ;; 读取至多 k 个字节，返回新 bytevector；
    ;; 文件结束且无字节可读时返回 eof-object。
    ;; 注：必须在此定义以遮蔽 reader.scm 内部的 #u8(...) 字面量读取函数。
    (define (read-bytevector k . maybe-port)
      (let* ((port (if (pair? maybe-port) (car maybe-port) (current-input-port)))
             (bv (make-bytevector k))
            ) ;
        (let loop
          ((i 0))
          (if (= i k)
            bv
            (let ((b (read-u8 port)))
              (if (eof-object? b)
                (if (zero? i) b (bytevector-copy bv 0 i))
                (begin
                  (bytevector-u8-set! bv i b)
                  (loop (+ i 1))
                ) ;begin
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

    (define-syntax let-values
      (syntax-rules ()
        ((let-values () body ...) (let () body ...))
        ((let-values ((vars init) rest ...) body ...)
         (call-with-values (lambda () init)
           (lambda vars (let-values (rest ...) body ...))))))

    (define-syntax define-values
      (lambda (stx)
        (syntax-case stx ()
          ((define-values (var ...) expr)
           (with-syntax (((tmp ...) (generate-temporaries #'(var ...))))
             #'(begin
                 (define var (quote uninitialized)) ...
                 (call-with-values (lambda () expr)
                   (lambda (tmp ...) (set! var tmp) ...))))))))

    (define-syntax define-record-type
      (lambda (stx)
        (let* ((form (syntax->datum stx))
               (type (cadr form))
               (make-datum (caddr form))
               (pred (cadddr form))
               (fields (cddddr form))
               (rtd (next-record-rtd))
               (make-name (car make-datum))
               (make-params (cdr make-datum))
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
          (datum->syntax stx
            `(begin
               (define ,rtd (make-record-type ',type ',field-names))
               ,@(if (keyword? type) '() `((define ,type ,rtd)))
               (define ,make-datum (vector ,rtd ,@make-params))
               (define (,pred obj)
                 (and (vector? obj) (positive? (vector-length obj)) (eq? (vector-ref obj 0) ,rtd)))
               ,@acc-defs
               ',type)))))
    (define-syntax guard-clauses
      (syntax-rules (else =>)
        ((_ fallback) fallback)
        ((_ fallback (else body ...)) (begin body ...))
        ((_ fallback (test => receiver) more ...)
         (let ((value test))
           (if value (receiver value) (guard-clauses fallback more ...))))
        ((_ fallback (test) more ...)
         (let ((value test))
           (if value value (guard-clauses fallback more ...))))
        ((_ fallback (test body ...) more ...)
         (if test (begin body ...) (guard-clauses fallback more ...)))))

    (define-syntax guard
      (syntax-rules ()
        ((_ (var clause ...) body ...)
         ((call/cc
            (lambda (exit)
              (with-exception-handler
                (lambda (condition)
                  ((call/cc
                     (lambda (resume)
                       (exit
                         (lambda ()
                           (let ((var condition))
                             (guard-clauses
                               (resume (lambda () (raise-continuable condition)))
                               clause ...))))))))
                (lambda ()
                  (call-with-values (lambda () body ...)
                    (lambda results (exit (lambda () (apply values results)))))))))))))

    ;; Symbol-tagged errors use the native error primitive. A string first
    ;; argument creates an R7RS error object for handlers to inspect.

    (define-record-type <error-object>
      (make-error-object message irritants)
      %scheme-error-object?
      (message %error-object-message)
      (irritants %error-object-irritants))

    (define (error message . irritants)
      (if (string? message)
        (raise (make-error-object message irritants))
        (apply host-error message irritants)))

    (define (error-object-message obj)
      (cond ((%scheme-error-object? obj) (%error-object-message obj))
            ((native-error-object? obj) (native-error-object-message obj))
            (else (host-error 'type-error "error-object-message: not an error object" obj))))

    (define (error-object-irritants obj)
      (cond ((%scheme-error-object? obj) (%error-object-irritants obj))
            ((native-error-object? obj) (%native-error-object-irritants obj))
            (else (host-error 'type-error "error-object-irritants: not an error object" obj))))

    (define (error-object? obj)
      (or (%scheme-error-object? obj) (native-error-object? obj)))

    (define-syntax include
      (lambda (stx)
        (syntax-case stx ()
          ((include filename ...)
           (let ((forms (apply append
                          (map (lambda (fn)
                                 (let ((port (open-input-file fn)))
                                   (let loop ((acc '()))
                                     (let ((form (read port)))
                                       (if (eof-object? form)
                                         (begin (close-port port) (reverse acc))
                                         (loop (cons form acc)))))))
                               (syntax->datum #'(filename ...))))))
             (datum->syntax stx `(begin ,@forms)))))))

    (define-syntax include-ci
      (lambda (stx)
        (syntax-case stx ()
          ((include-ci filename ...)
           (let ((forms (apply append
                          (map (lambda (fn)
                                 (let ((port (open-input-file fn)))
                                   (let loop ((acc '()))
                                     (let ((form (read port)))
                                       (if (eof-object? form)
                                         (begin (close-port port) (reverse acc))
                                         (loop (cons form acc)))))))
                               (syntax->datum #'(filename ...))))))
             (datum->syntax stx `(begin ,@forms)))))))
  ) ;begin
) ;define-library
