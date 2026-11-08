;;
;; Copyright (C) 2024-2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(define-library (scheme base)
  (export let-values
    let*-values
    define-syntax
    syntax-rules
    syntax-error
    let-syntax
    letrec-syntax
    ;; R7RS 5: Program Structure
    define-values
    define-record-type
    ;; R7RS 6.1: Equivalence predicates
    eqv?
    eq?
    equal?
    ;; R7RS 6.2: Numbers
    ;; - 比较和算术
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
    ;; - 数值函数
    square
    expt
    exact
    inexact
    max
    min
    floor
    floor/
    ceiling
    truncate
    truncate/
    truncate-quotient
    truncate-remainder
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
    ;; - 类型判断
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
    ;; R7RS 6.3: Booleans
    not
    boolean=?
    boolean?
    ;; R7RS 6.4: list
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
    ;; R7RS 6.5: Symbol
    symbol?
    symbol=?
    string->symbol
    symbol->string
    ;; R7RS 6.6: Characters
    char?
    char=?
    char<?
    char>?
    char<=?
    char>=?
    char->integer
    integer->char
    ;; R7RS 6.7: String
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
    string-copy!
    string->list
    list->string
    string=?
    string<?
    string>?
    string<=?
    string>=?
    ;; R7RS 6.8: Vector
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
    ;; R7RS 6.9: Bytevectors
    bytevector?
    make-bytevector
    bytevector
    bytevector-length
    bytevector-u8-ref
    bytevector-u8-set!
    bytevector-copy
    bytevector-copy!
    bytevector-append
    utf8->string
    string->utf8
    utf8-string-length
    bytevector-advance-utf8
    ;; Input and Output
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
    write-string
    newline
    flush-output-port
    eof-object?
    eof-object
    char-ready?
    with-input-from-file
    with-output-to-file
    ;; Control flow
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
    ;; Exception
    raise
    guard
    read-error?
    file-error?
  ) ;export
  (begin

    ;; R7RS 4.2.2: (let-values (<formals> <expr>) ...) 与
    ;; (let*-values (<formals> <expr>) ...) 把多值表达式的结果绑定到形参。
    ;;
    ;; 实现取自 SRFI-11 参考实现：
    ;;   Copyright (C) Lars Tansen (1999). All Rights Reserved.
    ;;   基于 Lars Tansen 编写、置于公共领域的 SRFI 11 参考实现。
    ;; 该版权头保留在 goldfish/srfi/srfi-11.scm 中；实现直接内联到
    ;; (scheme base)，避免 (scheme base) 反向依赖 (srfi srfi-11)。
    (define-syntax let-values
      (syntax-rules ()
        ((let-values () ?body0 ?body1 ...) (begin ?body0 ?body1 ...))

        ((let-values ((?formals ?expr))
           ?body0
           ?body1
           ...
         ) ;let-values
         (call-with-values (lambda () ?expr) (lambda ?formals ?body0 ?body1 ...))
        ) ;

        ((let-values (?binding0 ?binding1 ?binding2 ...)
           ?body0
           ?body1
           ...
         ) ;let-values
         (let-values "eval"
           (?binding0 ?binding1 ?binding2 ...)
           ()
           ()
           (begin
             ?body0
             ?body1
             ...
           ) ;begin
         ) ;let-values
        ) ;

        ((let-values "eval"
           ()
           (?formal ...)
           (?t ...)
           ?body
         ) ;let-values
         (let-values "apply"
           (?formal ...)
           (?t ...)
           ?body
         ) ;let-values
        ) ;

        ((let-values "eval"
           ((?f ?e) . ?more)
           (?formal ...)
           (?t ...)
           ?body
         ) ;let-values
         (call-with-values (lambda () ?e)
           (lambda t (let-values "eval" ?more (?formal ... ?f) (?t ... t) ?body))
         ) ;call-with-values
        ) ;

        ((let-values "apply" () () ?body) ?body)

        ((let-values "apply"
           (?f . ?f-more)
           (?t . ?t-more)
           ?body
         ) ;let-values
         (apply (lambda ?f (let-values "apply" ?f-more ?t-more ?body)) ?t)
        ) ;
      ) ;syntax-rules
    ) ;define-syntax

    (define-syntax let*-values
      (syntax-rules ()
        ((let*-values () ?body0 ?body1 ...) (begin ?body0 ?body1 ...))

        ((let*-values (?binding0 ?binding1 ...)
           ?body0
           ?body1
           ...
         ) ;let*-values
         (let-values (?binding0)
           (let*-values (?binding1 ...)
             ?body0
             ?body1
             ...
           ) ;let*-values
         ) ;let-values
        ) ;
      ) ;syntax-rules
    ) ;define-syntax

    ;; R7RS 6.4: (list? obj) 仅当 obj 是有限完全列表时返回 #t
    ;; 点对结尾（improper list）和循环列表都不是有限完全列表，返回 #f
    (define list? proper-list?)

    (define-macro (define-values vars expression)

      `(if (not (null? (quote ,vars)))
         (varlet (curlet) ((lambda ,vars (curlet)) ,expression)))
    ) ;define-macro

    (define exact inexact->exact)

    (define inexact exact->inexact)

    (define (max2 x y)
      (when (or (not (real? x)) (not (real? y)))
        (error 'type-error "max: parameter must be real number")
      ) ;when
      (if (or (inexact? x) (inexact? y)) (inexact (s7-max x y)) (s7-max x y))
    ) ;define

    (define (max x . xs)
      (let loop
        ((current-max x) (remaining xs))
        (if (null? remaining)
          current-max
          (loop (max2 current-max (car remaining)) (cdr remaining))
        ) ;if
      ) ;let
    ) ;define

    (define (min2 x y)
      (when (or (not (real? x)) (not (real? y)))
        (error 'type-error "min: parameter must be real number")
      ) ;when
      (if (or (inexact? x) (inexact? y)) (inexact (s7-min x y)) (s7-min x y))
    ) ;define

    (define (min x . xs)
      (let loop
        ((current-min x) (remaining xs))
        (if (null? remaining)
          current-min
          (loop (min2 current-min (car remaining)) (cdr remaining))
        ) ;if
      ) ;let
    ) ;define

    (define (floor x)
      (if (inexact? x) (inexact (s7-floor x)) (s7-floor x))
    ) ;define

    (define (ceiling x)
      (if (inexact? x) (inexact (s7-ceiling x)) (s7-ceiling x))
    ) ;define

    (define (truncate x)
      (if (inexact? x) (inexact (s7-truncate x)) (s7-truncate x))
    ) ;define

    (define (round x)
      (if (inexact? x) (inexact (s7-round x)) (s7-round x))
    ) ;define

    (define (floor-quotient x y)
      (floor (/ x y))
    ) ;define

    (define (floor/ x y)
      (when (or (not (real? x)) (not (real? y)))
        (error 'type-error "floor/: parameters must be real numbers")
      ) ;when
      (when (zero? y)
        (error 'division-by-zero "floor/: division by zero")
      ) ;when
      (let ((q (floor (/ x y))) (r (modulo x y)))
        (values q r)
      ) ;let
    ) ;define

    (define (floor-remainder x y)
      (when (or (not (real? x)) (not (real? y)))
        (error 'type-error "floor-remainder: parameters must be reals")
      ) ;when
      (when (zero? y)
        (error 'division-by-zero "floor-remainder: division by zero")
      ) ;when
      (modulo x y)
    ) ;define

    (define (truncate/ x y)
      (when (or (not (real? x)) (not (real? y)))
        (error 'type-error "truncate/: parameters must be real numbers")
      ) ;when
      (when (zero? y)
        (error 'division-by-zero "truncate/: division by zero")
      ) ;when
      (let* ((q (truncate (/ x y))) (r (- x (* q y))))
        (values q r)
      ) ;let*
    ) ;define

    (define (truncate-quotient x y)
      (when (or (not (real? x)) (not (real? y)))
        (error 'type-error "truncate-quotient: parameters must be real numbers")
      ) ;when
      (when (zero? y)
        (error 'division-by-zero "truncate-quotient: division by zero")
      ) ;when
      (truncate (/ x y))
    ) ;define

    (define (truncate-remainder x y)
      (when (or (not (real? x)) (not (real? y)))
        (error 'type-error "truncate-remainder: parameters must be reals")
      ) ;when
      (when (zero? y)
        (error 'division-by-zero "truncate-remainder: division by zero")
      ) ;when
      (let* ((q (truncate (/ x y))) (r (- x (* q y))))
        r
      ) ;let*
    ) ;define

    (define (modulo x y)
      (when (or (not (real? x)) (not (real? y)))
        (error 'type-error "modulo: parameters must be reals")
      ) ;when
      (when (zero? y)
        (error 'division-by-zero "modulo: division by zero")
      ) ;when
      (s7-modulo x y)
    ) ;define

    (define (lcm2 x y)
      (when (or (not (real? x)) (not (real? y)))
        (error 'type-error "lcm: parameters must be reals")
      ) ;when
      (cond
       ((and (inexact? x) (exact? y)) (inexact (s7-lcm (exact x) y)))
       ((and (exact? x) (inexact? y)) (inexact (s7-lcm x (exact y))))
       ((and (inexact? x) (inexact? y)) (inexact (s7-lcm (exact x) (exact y))))
       (else (s7-lcm x y))
      ) ;cond
    ) ;define

    (define (lcm . args)
      (cond ((null? args) 1)
            ((null? (cdr args)) (lcm2 (car args) 1))
            ((null? (cddr args)) (lcm2 (car args) (cadr args)))
            (else
              (apply lcm (cons (lcm (car args) (cadr args)) (cddr args)))
            ) ;else
      ) ;cond
    ) ;define

    (define (square x)
      (* x x)
    ) ;define

    (define (exact-integer-sqrt n)
      (when (not (integer? n))
        (type-error "n must be an integer" n)
      ) ;when
      (when (< n 0)
        (value-error "n must be non-negative" n)
      ) ;when
      (let* ((a (sqrt n)) (b (inexact->exact (floor a))) (square-b (square b)))
        (if (= square-b n) (values b 0) (values b (- n square-b)))
      ) ;let*
    ) ;define

    (define exact-integer? integer?)

    (define (boolean=? obj1 obj2 . rest)
      (define (same-boolean obj rest)
        (if (null? rest) #t (and (equal? obj (car rest)) (same-boolean obj (cdr rest))))
      ) ;define
      (cond ((not (boolean? obj1)) #f)
            ((not (boolean? obj2)) #f)
            ((not (equal? obj1 obj2)) #f)
            (else (same-boolean obj1 rest))
      ) ;cond
    ) ;define

    (define (symbol=? sym1 sym2 . rest)
      (define (same-symbol sym rest)
        (if (null? rest) #t (and (eq? sym (car rest)) (same-symbol sym (cdr rest))))
      ) ;define
      (cond ((not (symbol? sym1)) #f)
            ((not (symbol? sym2)) #f)
            ((not (eq? sym1 sym2)) #f)
            (else (same-symbol sym1 rest))
      ) ;cond
    ) ;define

    (define bytevector byte-vector)

    (define bytevector? byte-vector?)

    (define make-bytevector make-byte-vector)

    (define bytevector-length length)

    (define bytevector-u8-ref byte-vector-ref)

    (define bytevector-u8-set! byte-vector-set!)

    (define*
      (bytevector-copy v (start 0) (end (and (bytevector? v) (bytevector-length v))))
      (unless (bytevector? v)
        (error 'type-error "bytevector-copy: parameter must be a bytevector" v)
      ) ;unless
      (unless (and (integer? start) (integer? end))
        (error 'type-error "bytevector-copy: start and end must be integers")
      ) ;unless
      (if (or (< start 0) (> start end) (> end (bytevector-length v)))
        (error 'out-of-range "bytevector-copy")
      ) ;if
      (let ((new-v (make-bytevector (- end start))))
        (let loop
          ((i start) (j 0))
          (if (>= i end)
            new-v
            (begin
              (bytevector-u8-set! new-v j (bytevector-u8-ref v i))
              (loop (+ i 1) (+ j 1))
            ) ;begin
          ) ;if
        ) ;let
      ) ;let
    ) ;define*

    ;; R7RS 6.9：(bytevector-copy! to at from [start [end]]) 把 from 的
    ;; [start, end) 区间复制到 to 的 at 起始位置。源与目标重叠时按
    ;; “先拷入临时字节向量再拷入目标”的语义处理。
    (define*
      (bytevector-copy! to
        at
        from
        (start 0)
        (end (and (bytevector? from) (bytevector-length from)))
      ) ;bytevector-copy!
      (unless (bytevector? to)
        (error 'type-error "bytevector-copy!: 'to' must be a bytevector" to)
      ) ;unless
      (unless (integer? at)
        (error 'type-error "bytevector-copy!: 'at' must be an integer" at)
      ) ;unless
      (unless (bytevector? from)
        (error 'type-error "bytevector-copy!: 'from' must be a bytevector" from)
      ) ;unless
      (unless (and (integer? start) (integer? end))
        (error 'type-error "bytevector-copy!: 'start' and 'end' must be integers")
      ) ;unless
      (if
        (or (< at 0)
          (> at (bytevector-length to))
          (< start 0)
          (> start (bytevector-length from))
          (< end 0)
          (> end (bytevector-length from))
          (> start end)
          (> (+ at (- end start)) (bytevector-length to))
        ) ;or
        (error 'out-of-range "bytevector-copy!")
        ;; 先拷到临时字节向量，再写入目标，天然满足重叠语义
        (let ((tmp (bytevector-copy from start end)))
          (let loop
            ((to-i at) (from-i 0))
            (if (>= from-i (bytevector-length tmp))
              to
              (begin
                (bytevector-u8-set! to to-i (bytevector-u8-ref tmp from-i))
                (loop (+ to-i 1) (+ from-i 1))
              ) ;begin
            ) ;if
          ) ;let
        ) ;let
      ) ;if
    ) ;define*

    (define (bytevector-append . args)
      (for-each (lambda (bv)
                  (unless (bytevector? bv)
                    (error 'type-error "bytevector-append: argument must be a bytevector" bv)
                  ) ;unless
                ) ;lambda
        args
      ) ;for-each
      (apply append args)
    ) ;define

    (define* (bytevector-advance-utf8 bv index (end (length bv)))
      (if (>= index end)
        index
        (let ((byte (bv index)))
          (cond
            ;; 1-byte sequence (0xxxxxxx)
            ((< byte 128) (+ index 1))

            ;; 2-byte sequence (110xxxxx 10xxxxxx)
            ((< byte 224)
             (if (>= (+ index 1) end)
               index
               (let ((next-byte (bv (+ index 1))))
                 (if (not (= (logand next-byte 192) 128)) index (+ index 2))
               ) ;let
             ) ;if
            ) ;

            ;; 3-byte sequence (1110xxxx 10xxxxxx 10xxxxxx)
            ((< byte 240)
             (if (>= (+ index 2) end)
               index
               (let ((next-byte1 (bv (+ index 1))) (next-byte2 (bv (+ index 2))))
                 (if
                   (or (not (= (logand next-byte1 192) 128)) (not (= (logand next-byte2 192) 128)))
                   index
                   (+ index 3)
                 ) ;if
               ) ;let
             ) ;if
            ) ;

            ;; 4-byte sequence (11110xxx 10xxxxxx 10xxxxxx 10xxxxxx)
            ((< byte 248)
             (if (>= (+ index 3) end)
               index
               (let ((next-byte1 (bv (+ index 1)))
                     (next-byte2 (bv (+ index 2)))
                     (next-byte3 (bv (+ index 3)))
                    ) ;
                 (if
                   (or (not (= (logand next-byte1 192) 128))
                     (not (= (logand next-byte2 192) 128))
                     (not (= (logand next-byte3 192) 128))
                   ) ;or
                   index
                   (+ index 4)
                 ) ;if
               ) ;let
             ) ;if
            ) ;
            (else index)
          ) ;cond
        ) ;let
      ) ;if
    ) ;define*

    (define (utf8-string-length str)
      (let ((bv (string->byte-vector str)) (N (string-length str)))
        (if (zero? N)
          0
          (let loop
            ((pos 0) (cnt 0))
            (let ((next-pos (bytevector-advance-utf8 bv pos N)))
              (cond ((= next-pos N) (+ cnt 1))
                    ((= next-pos pos) (error 'value-error "Invalid UTF-8 sequence at index: " pos))
                    (else (loop next-pos (+ cnt 1)))
              ) ;cond
            ) ;let
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define* (utf8->string bv (start 0) (end (bytevector-length bv)))
      (if (or (< start 0) (> end (bytevector-length bv)) (> start end))
        (error 'out-of-range start end)
        (let loop
          ((pos start))
          (let ((next-pos (bytevector-advance-utf8 bv pos end)))
            (cond
             ((= next-pos end) (copy bv (make-string (- end start)) start end))
             ((= next-pos pos) (error 'value-error "Invalid UTF-8 sequence at index: " pos))
             (else (loop next-pos))
            ) ;cond
          ) ;let
        ) ;let
      ) ;if
    ) ;define*

    (define* (string->utf8 str (start 0) (end #t)) (g_string->utf8 str start end))

    (define (raise . args)
      (apply throw #t args)
    ) ;define

    ;; guard 的卫生基础设施（Issue 1341）。
    ;;
    ;; 旧 define-macro 实现的展开产物里含裸的 cond/else/apply/throw，会被使用点
    ;; 同名局部绑定遮蔽（如 (guard (cond ...) ...) 直接崩溃）。本层把 guard 的
    ;; 全部运行时机制封装进以下闭包：自由标识符在本库定义环境中解析，
    ;; 对使用点遮蔽免疫；宏展开产物只嵌入对这些闭包的过程对象引用，
    ;; 不携带任何裸的特殊形式或过程名。
    ;;
    ;; 异常协议：body 正常 → 值原样透传（含多值）；异常 → 打包为
    ;; (list %guard-tag args)，作为唯一返回值与正常路径区分。

    (define %guard-tag (list 'guard 'tag))

    ;; 执行 body：异常时打包标记。S7 会复用/改写 catch handler 的参数列表，
    ;; 且内层 payload 列表与异常结构共享（跨求值步骤会被改写为循环结构），
    ;; 故逐层拷贝两层结构。
    (define (%guard-try body-thunk)
      (catch #t
        body-thunk
        (lambda args
          (list %guard-tag
            (map (lambda (x) (if (pair? x) (list-copy x) x)) (list-copy args))
          ) ;list
        ) ;lambda
      ) ;catch
    ) ;define

    ;; 消费 %guard-try 结果：正常 → 原样透传；打包值 → 交 dispatch 分发，
    ;; 其返回值为 guard 表达式的值；dispatch 未匹配时应重新抛出原异常。
    (define (%guard-run body-thunk dispatch)
      (call-with-values (lambda () (%guard-try body-thunk))
        (lambda vals
          (if (and (pair? vals)
                (null? (cdr vals))
                (pair? (car vals))
                (eq? (caar vals) %guard-tag)
              ) ;and
            (dispatch (cadr (car vals)))
            (apply values vals)
          ) ;if
        ) ;lambda
      ) ;call-with-values
    ) ;define

    ;; 从打包的异常参数中提取条件值：caught-args 形如 (tag (payload ...))，
    ;; guard 变量绑定到首个 payload（raise 'b → b；error 'k "m" 42 → "m"）。
    (define (%guard-condition caught-args)
      (if (and (pair? caught-args) (pair? (cdr caught-args)) (pair? (cadr caught-args)))
        (car (cadr caught-args))
        (if (pair? caught-args) (car caught-args) caught-args)
      ) ;if
    ) ;define

    ;; 恢复并重新抛出打包的异常参数（re-raise 语义）。
    ;; caught-args 形如 (tag (payload ...))，还原为原始 (throw tag payload ...)。
    (define (%guard-reraise caught-args)
      (apply throw (car caught-args) (cadr caught-args))
    ) ;define

    ;; 按序驱动子句链：clauses 是扁平列表 (test1 handler1 test2 handler2 ...)，
    ;; test 是 (lambda (var) ...)，var 为条件值；test 求值 #f 则链下一子句；
    ;; test 非假时把 test 值与异常参数一并传给 handler（=> 子句用 test 值，
    ;; 普通子句忽略）；链尾 reraise。
    ;; 全部控制流（if/apply/throw）在本闭包内，免疫使用点遮蔽。
    (define (%guard-dispatch caught-args clauses)
      (if (null? clauses)
        (%guard-reraise caught-args)
        (let ((t ((car clauses) (%guard-condition caught-args))))
          (if t
           ((cadr clauses) caught-args t)
           (%guard-dispatch caught-args (cddr clauses))
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define-macro (guard results . body)
      (unless (and (pair? results) (symbol? (car results)))
        (error 'syntax-error "guard: malformed results specification" results)
      ) ;unless
      (let* ((var (car results))
             (clauses (cdr results))
             (dispatcher %guard-dispatch)
             (runner %guard-run)
            ) ;
        ;; 每个子句编译为 (test-lambda handler-lambda) 两元素，全部子句拼成
        ;; 扁平列表 (list t1 h1 t2 h2 ...)。特殊形式（=> / else）在编译期
        ;; 归一化：else → 测试恒真；=> → handler 内调用 receiver。
        ;; 闭包体内只引用宏改名的 var 与用户表达式，控制流全部交给 dispatcher。
        (define (compile-one-clause clause)
          (cond
           ((eq? (car clause) 'else)
            (list
              `(lambda (,var) #t)
              `(lambda (caught-args t)
                 ((lambda (,var) ,@(cdr clause))
                  (,%guard-condition caught-args)))
            ) ;list
           ) ;
           ((and (pair? (cdr clause)) (eq? (cadr clause) '=>))
            (list
              `(lambda (,var) ,(car clause))
              `(lambda (caught-args t)
                 ((lambda (,var) (,(caddr clause) t))
                  (,%guard-condition caught-args)))
            ) ;list
           ) ;
           (else
             (list
               `(lambda (,var) ,(car clause))
               `(lambda (caught-args t)
                  ((lambda (,var) (begin ,@(cdr clause)))
                   (,%guard-condition caught-args)))
             ) ;list
           ) ;else
          ) ;cond
        ) ;define
        (define (compile-clauses cls)
          (if (null? cls) ''() `(list ,@(apply append
                                          (map compile-one-clause cls))))
        ) ;define
        `(,runner
          (lambda ,() ,@body)
          (lambda (caught-args)
            (,dispatcher caught-args ,(compile-clauses clauses))))
      ) ;let*
    ) ;define-macro

    (define (read-error? obj)
      (eq? (car obj) 'read-error)
    ) ;define

    (define (file-error? obj)
      (eq? (car obj) 'io-error)
    ) ;define

    (define (call-with-port port proc)
      (let ((res (proc port)))
        (if res (close-port port))
        res
      ) ;let
    ) ;define

    (define (port? p)
      (or (input-port? p) (output-port? p))
    ) ;define

    (define textual-port? port?)

    (define binary-port? port?)

    (define (input-port-open? p)
      (not (port-closed? p))
    ) ;define

    (define (output-port-open? p)
      (not (port-closed? p))
    ) ;define

    (define (close-port p)
      (if (input-port? p) (close-input-port p) (close-output-port p))
    ) ;define

    (define (eof-object)
      #<eof>
    ) ;define

    (define list-copy copy)

    (define (string-copy str . start_end)
      (cond ((null? start_end) (substring str 0))
            ((= (length start_end) 1) (substring str (car start_end)))
            ((= (length start_end) 2) (substring str (car start_end) (cadr start_end)))
            (else (error 'wrong-number-of-args))
      ) ;cond
    ) ;define

    (define (string-map p . args)
      (apply string (apply map p args))
    ) ;define

    (define string-for-each for-each)

    (define*
      (vector-copy v (start 0) (end (and (vector? v) (vector-length v))))
      (if (not (vector? v))
        (error 'type-error "vector-copy: parameter must be a vector")
        (if (or (not (integer? start)) (not (integer? end)))
          (error 'type-error "vector-copy: start and end must be integers")
          (if (or (< start 0) (> start end) (> end (vector-length v)))
            (error 'out-of-range "vector-copy")
            (let ((new-v (make-vector (- end start))))
              (let loop
                ((i start) (j 0))
                (if (>= i end)
                  new-v
                  (begin
                    (vector-set! new-v j (vector-ref v i))
                    (loop (+ i 1) (+ j 1))
                  ) ;begin
                ) ;if
              ) ;let
            ) ;let
          ) ;if
        ) ;if
      ) ;if
    ) ;define*

    (define (vector-map p . args)
      (if (not (procedure? p))
        (error 'type-error "vector-map: first parameter must be a procedure")
      ) ;if
      (if (null? args)
        (error 'wrong-number-of-args "vector-map: requires at least one vector")
      ) ;if
      (for-each
        (lambda (v)
          (if (not (vector? v))
            (error 'type-error "vector-map: parameter must be a vector")
          ) ;if
        ) ;lambda
        args
      ) ;for-each
      (apply vector (apply map p args))
    ) ;define

    (define (vector-for-each p . args)
      (if (not (procedure? p))
        (error 'type-error "vector-for-each: first parameter must be a procedure")
      ) ;if
      (if (null? args)
        (error 'wrong-number-of-args "vector-for-each: requires at least one vector")
      ) ;if
      (for-each
        (lambda (v)
          (if (not (vector? v))
            (error 'type-error "vector-for-each: parameter must be a vector")
          ) ;if
        ) ;lambda
        args
      ) ;for-each
      (apply for-each p args)
    ) ;define

    (define (vector-fill! vec fill . rest)
      (unless (vector? vec)
        (error 'type-error "vector-fill!: first argument must be a vector" vec)
      ) ;unless
      (unless (null? rest)
        (unless (integer? (car rest))
          (error 'type-error "vector-fill!: start must be an integer" (car rest))
        ) ;unless
        (unless (null? (cdr rest))
          (unless (integer? (cadr rest))
            (error 'type-error "vector-fill!: end must be an integer" (cadr rest))
          ) ;unless
        ) ;unless
      ) ;unless
      (apply fill! vec fill rest)
    ) ;define

    (define*
      (vector-copy! to
        at
        from
        (start 0)
        (end (and (vector? from) (vector-length from)))
      ) ;vector-copy!
      (if (not (vector? to))
        (error 'type-error "vector-copy!: 'to' must be a vector")
        (if (not (integer? at))
          (error 'type-error "vector-copy!: 'at' must be an integer")
          (if (not (vector? from))
            (error 'type-error "vector-copy!: 'from' must be a vector")
            (if (or (not (integer? start)) (not (integer? end)))
              (error 'type-error "vector-copy!: 'start' and 'end' must be integers")
              (if
                (or (< at 0)
                  (< start 0)
                  (> start (vector-length from))
                  (< end 0)
                  (> end (vector-length from))
                  (> start end)
                  (> (+ at (- end start)) (vector-length to))
                ) ;or
                (error 'out-of-range "vector-copy!")
                (let loop
                  ((to-i at) (from-i start))
                  (if (>= from-i end)
                    to
                    (begin
                      (vector-set! to to-i (vector-ref from from-i))
                      (loop (+ to-i 1) (+ from-i 1))
                    ) ;begin
                  ) ;if
                ) ;let
              ) ;if
            ) ;if
          ) ;if
        ) ;if
      ) ;if
    ) ;define*

    ;; R7RS 6.7：(string-copy! to at from [start [end]]) 把 from 的
    ;; [start, end) 区间复制到 to 的 at 起始位置。源与目标重叠时按
    ;; “先拷入临时串再拷入目标”的语义处理。
    (define*
      (string-copy! to
        at
        from
        (start 0)
        (end (and (string? from) (string-length from)))
      ) ;string-copy!
      (if (not (string? to))
        (error 'type-error "string-copy!: 'to' must be a string")
        (if (not (integer? at))
          (error 'type-error "string-copy!: 'at' must be an integer")
          (if (not (string? from))
            (error 'type-error "string-copy!: 'from' must be a string")
            (if (or (not (integer? start)) (not (integer? end)))
              (error 'type-error "string-copy!: 'start' and 'end' must be integers")
              (if
                (or (< at 0)
                  (> at (string-length to))
                  (< start 0)
                  (> start (string-length from))
                  (< end 0)
                  (> end (string-length from))
                  (> start end)
                  (> (+ at (- end start)) (string-length to))
                ) ;or
                (error 'out-of-range "string-copy!")
                ;; 先拷到临时串，再写入目标，天然满足重叠语义
                (let ((tmp (substring from start end)))
                  (let loop
                    ((to-i at) (from-i 0))
                    (if (>= from-i (string-length tmp))
                      to
                      (begin
                        (string-set! to to-i (string-ref tmp from-i))
                        (loop (+ to-i 1) (+ from-i 1))
                      ) ;begin
                    ) ;if
                  ) ;let
                ) ;let
              ) ;if
            ) ;if
          ) ;if
        ) ;if
      ) ;if
    ) ;define*

    ;; 0-clause BSD
    ;; Bill Schottstaedt
    ;; from S7 source repo: r7rs.scm
    (define* (vector->string v (start 0) end)
      (if (null? v)
        ""
        (if (not (vector? v))
          (error 'type-error "vector->string: first argument must be a vector")
          (let ((stop (or end (vector-length v))))
            (if (or (not (integer? start)) (not (integer? stop)))
              (error 'type-error "vector->string: start and end must be integers")
              (if (or (< start 0) (> start stop) (> stop (vector-length v)))
                (error 'out-of-range "vector->string: index out of range")
                (let ((s (make-string (- stop start))))
                  (let loop
                    ((i start) (j 0))
                    (if (>= i stop)
                      s
                      (let ((c (vector-ref v i)))
                        (if (not (char? c))
                          (error 'type-error "vector->string: elements must be characters")
                          (begin
                            (string-set! s j c)
                            (loop (+ i 1) (+ j 1))
                          ) ;begin
                        ) ;if
                      ) ;let
                    ) ;if
                  ) ;let
                ) ;let
              ) ;if
            ) ;if
          ) ;let
        ) ;if
      ) ;if
    ) ;define*

    ;; 0-clause BSD
    ;; Bill Schottstaedt
    ;; from S7 source repo: r7rs.scm
    (define* (string->vector s (start 0) end)
      (if (not (or (string? s) (vector? s) (list? s) (bytevector? s)))
        (error 'type-error "string->vector: first argument must be a sequence")
        (let ((stop (or end (length s))))
          (if (or (not (integer? start)) (not (integer? stop)))
            (error 'type-error "string->vector: start and end must be integers")
            (if (or (< start 0) (> start stop) (> stop (length s)))
              (error 'out-of-range "string->vector: index out of range")
              (copy s (make-vector (- stop start)) start stop)
            ) ;if
          ) ;if
        ) ;let
      ) ;if
    ) ;define*

  ) ;begin
) ;define-library
