;;; host-abi.scm -- the R7RS value surface the s7 host lacks, or whose
;;; semantics differ (exact-ness of floor/ceiling, list-based set ops, ...).
;;; Loaded during the s7 bootstrap phase (liii_reader.cpp) into the rootlet,
;;; where the expander's primitive references and the artifact's re-bindings
;;; resolve at runtime.  eof-object / vector-map / vector-for-each are NOT
;;; defined here: the kernel (substrate.scm) provides them, and its
;;; re-bindings override anything this file could define.
(define exact inexact->exact)
(define inexact exact->inexact)
(define (max2 x y) (when (or (not (real? x)) (not (real? y))) (error (quote type-error) "max: parameter must be real number")) (if (or (inexact? x) (inexact? y)) (inexact (s7-max x y)) (s7-max x y)))
(define (max x . xs) (let loop ((current-max x) (remaining xs)) (if (null? remaining) current-max (loop (max2 current-max (car remaining)) (cdr remaining)))))
(define (min2 x y) (when (or (not (real? x)) (not (real? y))) (error (quote type-error) "min: parameter must be real number")) (if (or (inexact? x) (inexact? y)) (inexact (s7-min x y)) (s7-min x y)))
(define (min x . xs) (let loop ((current-min x) (remaining xs)) (if (null? remaining) current-min (loop (min2 current-min (car remaining)) (cdr remaining)))))
(define (floor x) (if (inexact? x) (inexact (s7-floor x)) (s7-floor x)))
(define (ceiling x) (if (inexact? x) (inexact (s7-ceiling x)) (s7-ceiling x)))
(define (truncate x) (if (inexact? x) (inexact (s7-truncate x)) (s7-truncate x)))
(define (round x) (if (inexact? x) (inexact (s7-round x)) (s7-round x)))
(define (floor-quotient x y) (floor (/ x y)))
(define (floor/ x y) (when (or (not (real? x)) (not (real? y))) (error (quote wrong-type-arg) "floor/: parameters must be real numbers")) (when (zero? y) (error (quote division-by-zero) "floor/: division by zero")) (let ((q (floor (/ x y))) (r (modulo x y))) (values q r)))
(define (floor-remainder x y) (when (or (not (real? x)) (not (real? y))) (error (quote type-error) "floor-remainder: parameters must be reals")) (when (zero? y) (error (quote division-by-zero) "floor-remainder: division by zero")) (modulo x y))
(define (truncate/ x y) (when (or (not (real? x)) (not (real? y))) (error (quote wrong-type-arg) "truncate/: parameters must be real numbers")) (when (zero? y) (error (quote division-by-zero) "truncate/: division by zero")) (let* ((q (truncate (/ x y))) (r (- x (* q y)))) (values q r)))
(define (modulo x y) (when (or (not (real? x)) (not (real? y))) (error (quote type-error) "modulo: parameters must be reals")) (when (zero? y) (error (quote division-by-zero) "modulo: division by zero")) (s7-modulo x y))
(define (lcm2 x y) (when (or (not (real? x)) (not (real? y))) (error (quote type-error) "lcm: parameters must be reals")) (cond ((and (inexact? x) (exact? y)) (inexact (s7-lcm (exact x) y))) ((and (exact? x) (inexact? y)) (inexact (s7-lcm x (exact y)))) ((and (inexact? x) (inexact? y)) (inexact (s7-lcm (exact x) (exact y)))) (else (s7-lcm x y))))
(define (lcm . args) (cond ((null? args) 1) ((null? (cdr args)) (lcm2 (car args) 1)) ((null? (cddr args)) (lcm2 (car args) (cadr args))) (else (apply lcm (cons (lcm (car args) (cadr args)) (cddr args))))))
(define (square x) (* x x))
(define (exact-integer-sqrt n) (when (not (integer? n)) (error (quote type-error) "n must be an integer" n)) (when (< n 0) (error (quote value-error) "n must be non-negative" n)) (let* ((a (sqrt n)) (b (inexact->exact (floor a))) (square-b (square b))) (if (= square-b n) (values b 0) (values b (- n square-b)))))
(define exact-integer? integer?)
(define (boolean=? obj1 obj2 . rest) (define (same-boolean obj rest) (if (null? rest) #t (and (equal? obj (car rest)) (same-boolean obj (cdr rest))))) (cond ((not (boolean? obj1)) #f) ((not (boolean? obj2)) #f) ((not (equal? obj1 obj2)) #f) (else (same-boolean obj1 rest))))
(define (symbol=? sym1 sym2 . rest) (define (same-symbol sym rest) (if (null? rest) #t (and (eq? sym (car rest)) (same-symbol sym (cdr rest))))) (cond ((not (symbol? sym1)) #f) ((not (symbol? sym2)) #f) ((not (eq? sym1 sym2)) #f) (else (same-symbol sym1 rest))))
(define bytevector byte-vector)
(define bytevector? byte-vector?)
(define make-bytevector make-byte-vector)
(define bytevector-length length)
(define bytevector-u8-ref byte-vector-ref)
(define bytevector-u8-set! byte-vector-set!)
(define* (bytevector-copy v (start 0) (end (bytevector-length v))) (if (or (< start 0) (> start end) (> end (bytevector-length v))) (error (quote out-of-range) "bytevector-copy")) (let ((new-v (make-bytevector (- end start)))) (let loop ((i start) (j 0)) (if (>= i end) new-v (begin (bytevector-u8-set! new-v j (bytevector-u8-ref v i)) (loop (+ i 1) (+ j 1)))))))
(define bytevector-append append)
(define* (bytevector-advance-utf8 bv index (end (length bv))) (if (>= index end) index (let ((byte (bv index))) (cond ((< byte 128) (+ index 1)) ((< byte 224) (if (>= (+ index 1) end) index (let ((next-byte (bv (+ index 1)))) (if (not (= (logand next-byte 192) 128)) index (+ index 2))))) ((< byte 240) (if (>= (+ index 2) end) index (let ((next-byte1 (bv (+ index 1))) (next-byte2 (bv (+ index 2)))) (if (or (not (= (logand next-byte1 192) 128)) (not (= (logand next-byte2 192) 128))) index (+ index 3))))) ((< byte 248) (if (>= (+ index 3) end) index (let ((next-byte1 (bv (+ index 1))) (next-byte2 (bv (+ index 2))) (next-byte3 (bv (+ index 3)))) (if (or (not (= (logand next-byte1 192) 128)) (not (= (logand next-byte2 192) 128)) (not (= (logand next-byte3 192) 128))) index (+ index 4))))) (else index)))))
(define (utf8-string-length str) (let ((bv (string->byte-vector str)) (N (string-length str))) (if (zero? N) 0 (let loop ((pos 0) (cnt 0)) (let ((next-pos (bytevector-advance-utf8 bv pos N))) (cond ((= next-pos N) (+ cnt 1)) ((= next-pos pos) (error (quote value-error) "Invalid UTF-8 sequence at index: " pos)) (else (loop next-pos (+ cnt 1)))))))))
(define* (utf8->string bv (start 0) (end (bytevector-length bv))) (if (or (< start 0) (> end (bytevector-length bv)) (> start end)) (error (quote out-of-range) start end) (let loop ((pos start)) (let ((next-pos (bytevector-advance-utf8 bv pos end))) (cond ((= next-pos end) (copy bv (make-string (- end start)) start end)) ((= next-pos pos) (error (quote value-error) "Invalid UTF-8 sequence at index: " pos)) (else (loop next-pos)))))))
(define* (string->utf8 str (start 0) (end #t)) (g_string->utf8 str start end))
(define (raise . args) (apply throw #t args))
(define (read-error? obj) (eq? (car obj) (quote read-error)))
(define (file-error? obj) (eq? (car obj) (quote io-error)))
(define (call-with-port port proc) (let ((res (proc port))) (if res (close-port port)) res))
(define (port? p) (or (input-port? p) (output-port? p)))
(define open-binary-input-file open-input-file)
(define open-binary-output-file open-output-file)
(define textual-port? port?)
(define binary-port? port?)
(define (input-port-open? p) (not (port-closed? p)))
(define (output-port-open? p) (not (port-closed? p)))
(define (close-port p) (if (input-port? p) (close-input-port p) (close-output-port p)))
(define list-copy copy)
(define (string-copy str . start_end) (cond ((null? start_end) (substring str 0)) ((= (length start_end) 1) (substring str (car start_end))) ((= (length start_end) 2) (substring str (car start_end) (cadr start_end))) (else (error (quote wrong-number-of-args)))))
(define (string-map p . args) (apply string (apply map p args)))
(define string-for-each for-each)
(define* (vector-copy v (start 0) (end (vector-length v))) (if (or (> start end) (> end (vector-length v))) (error (quote out-of-range) "vector-copy") (let ((new-v (make-vector (- end start)))) (let loop ((i start) (j 0)) (if (>= i end) new-v (begin (vector-set! new-v j (vector-ref v i)) (loop (+ i 1) (+ j 1))))))))
(define vector-fill! fill!)
(define* (vector-copy! to at from (start 0) (end (vector-length from))) (if (or (< at 0) (> start (vector-length from)) (< end 0) (> end (vector-length from)) (> start end) (> (+ at (- end start)) (vector-length to))) (error (quote out-of-range) "vector-copy!") (let loop ((to-i at) (from-i start)) (if (>= from-i end) to (begin (vector-set! to to-i (vector-ref from from-i)) (loop (+ to-i 1) (+ from-i 1)))))))
(define* (vector->string v (start 0) end) (let ((stop (or end (length v)))) (copy v (make-string (- stop start)) start stop)))
(define* (string->vector s (start 0) end) (let ((stop (or end (length s)))) (copy s (make-vector (- stop start)) start stop)))

;;; ---------------------------------------------------------------------------
;;; R7RS names the s7 host lacks entirely (primitive-by-name fallback).
;;; ---------------------------------------------------------------------------

;;; ---- math / numbers ----------------------------------------------

(define (finite? x)
  (and (number? x) (not (infinite? x)) (not (nan? x))))

(define (truncate-quotient x y)
  (truncate (/ x y)))

(define (truncate-remainder x y)
  (- x (* (truncate-quotient x y) y)))

;;; ---- characters ---------------------------------------------------

(define char-upcase g_char-upcase)
(define char-downcase g_char-downcase)
(define char-alphabetic? g_char-alphabetic?)
(define char-upper-case? g_char-upper-case?)
(define char-lower-case? g_char-lower-case?)

(define (char-numeric? c)
  (and (char? c)
       (let ((n (char->integer c)))
         (or (and (>= n 48) (<= n 57))
             (and (>= n 1632) (<= n 1641))
             (and (>= n 1776) (<= n 1785))
             (and (>= n 2406) (<= n 2415))))))

(define (char-whitespace? c)
  (and (char? c)
       (let ((n (char->integer c)))
         (or (memv n '(9 10 11 12 13 32))
             (and (>= n 160) (<= n 160))))))

;;; R7RS char-foldcase: downcase, then the small foldcase exceptions.
(define (char-foldcase c)
  (let ((cp (char->integer c)))
    (case cp
      ((181 304 383 837) (integer->char 956))
      ((962 976 977 981 982) (integer->char 963))
      ((1008) (integer->char 954))
      ((1009) (integer->char 961))
      ((1013) (integer->char 949))
      ((5024 5025 5026 5027 5028 5029 5030 5031 5032 5033 5034 5035 5036
        5037 5038 5039 5040 5041 5042 5043 5044 5045 5046 5047 5048 5049
        5050 5051 5052 5053 5054 5055 5056 5057 5058 5059 5060 5061 5062
        5063 5064 5065 5066 5067 5068 5069 5070 5071 5072 5073 5074 5075
        5076 5077 5078 5079 5080 5081 5082 5083 5084 5085 5086 5087 5088
        5089 5090 5091 5092 5093 5094 5095 5096 5097 5098 5099 5100 5101
        5102 5103 5104 5105 5106 5107 5108 5109)
       (integer->char cp))
      ((5112 5113 5114 5115 5116 5117) (integer->char (+ cp -8)))
      (else (char-downcase c)))))

(define (char-ci=? a b) (char=? (char-foldcase a) (char-foldcase b)))
(define (char-ci<? a b) (char<? (char-foldcase a) (char-foldcase b)))
(define (char-ci>? a b) (char>? (char-foldcase a) (char-foldcase b)))
(define (char-ci<=? a b) (char<=? (char-foldcase a) (char-foldcase b)))
(define (char-ci>=? a b) (char>=? (char-foldcase a) (char-foldcase b)))

;;; ---- strings ------------------------------------------------------

(define (string-upcase str)
  (list->string (map char-upcase (string->list str))))

(define (string-downcase str)
  (list->string (map char-downcase (string->list str))))

(define (string-foldcase str)
  (list->string (map char-foldcase (string->list str))))

(define (string-copy! to at from . range)
  (let* ((start (if (pair? range) (car range) 0))
         (end (if (and (pair? range) (pair? (cdr range)))
                 (cadr range)
                 (string-length from)))
         (n (- end start)))
    (let loop ((i 0))
      (unless (= i n)
        (string-set! to (+ at i) (string-ref from (+ start i)))
        (loop (+ i 1))))
    to))

(define (string-ci-compare cmp a b)
  (cmp (string-foldcase a) (string-foldcase b)))

(define (string-ci=? a b) (string-ci-compare string=? a b))
(define (string-ci<? a b) (string-ci-compare string<? a b))
(define (string-ci>? a b) (string-ci-compare string>? a b))
(define (string-ci<=? a b) (string-ci-compare string<=? a b))
(define (string-ci>=? a b) (string-ci-compare string>=? a b))

;;; ---- bytevectors --------------------------------------------------

(define (bytevector->u8-list bv)
  (let loop ((i (- (bytevector-length bv) 1)) (acc '()))
    (if (< i 0)
      acc
      (loop (- i 1) (cons (bytevector-u8-ref bv i) acc)))))

(define (u8-list->bytevector lst)
  (let* ((bv (make-bytevector (length lst))))
    (let loop ((i 0) (l lst))
      (if (null? l)
        bv
        (begin (bytevector-u8-set! bv i (car l))
               (loop (+ i 1) (cdr l)))))))

(define (bytevector-copy! to at from . range)
  (let* ((start (if (pair? range) (car range) 0))
         (end (if (and (pair? range) (pair? (cdr range)))
                 (cadr range)
                 (bytevector-length from)))
         (n (- end start)))
    (let loop ((i 0))
      (unless (= i n)
        (bytevector-u8-set! to (+ at i) (bytevector-u8-ref from (+ start i)))
        (loop (+ i 1))))
    to))

;;; ---- promises -----------------------------------------------------

;;; R7RS make-promise returns obj unchanged when obj is already a promise;
;;; otherwise it wraps obj in a lazy promise (see boot.scm's
;;; make-lazy-promise / force for the representation and semantics).
(define (make-promise obj)
  (if (promise? obj) obj (make-lazy-promise (lambda () obj))))

(define (promise? x)
  (and (pair? x) (pair? (cdr x)) (eq? (cadr x) '+promise+)))

;;; ---- ports: binary I/O --------------------------------------------

;;; goldfish does not distinguish textual/binary ports; binary I/O rides on
;;; s7's char-based ports (a byte is a character with code point 0-255).

(define (read-u8 . maybe-port)
  (let ((p (if (pair? maybe-port) (car maybe-port) (current-input-port))))
    (let ((c (read-char p)))
      (if (eof-object? c) c (char->integer c)))))

(define (peek-u8 . maybe-port)
  (let ((p (if (pair? maybe-port) (car maybe-port) (current-input-port))))
    (let ((c (peek-char p)))
      (if (eof-object? c) c (char->integer c)))))

(define (write-u8 byte . maybe-port)
  (let ((p (if (pair? maybe-port) (car maybe-port) (current-output-port))))
    (write-char (integer->char byte) p)))

(define (write-bytevector bv . rest)
  (let* ((port (if (and (pair? rest) (output-port? (car rest)))
                  (car rest)
                  (current-output-port)))
         (start (if (and (pair? rest) (not (output-port? (car rest)))) (car rest) 0))
         (end (cond ((and (pair? rest) (not (output-port? (car rest))) (pair? (cdr rest)))
                     (cadr rest))
                    (else (bytevector-length bv)))))
    (let loop ((i start))
      (unless (= i end)
        (write-u8 (bytevector-u8-ref bv i) port)
        (loop (+ i 1))))))

(define (read-bytevector! bv . rest)
  (let* ((port (if (and (pair? rest) (input-port? (car rest)))
                  (car rest)
                  (current-input-port)))
         (start (if (and (pair? rest) (not (input-port? (car rest)))) (car rest) 0))
         (end (cond ((and (pair? rest) (not (input-port? (car rest))) (pair? (cdr rest)))
                     (cadr rest))
                    (else (bytevector-length bv)))))
    (let loop ((i start))
      (if (< i end)
        (let ((b (read-u8 port)))
          (if (eof-object? b)
            (- i start)
            (begin (bytevector-u8-set! bv i b)
                   (loop (+ i 1)))))
        (- end start)))))

(define (open-input-bytevector bv)
  (open-input-string
    (list->string (map (lambda (n) (integer->char n))
                       (bytevector->u8-list bv)))))

(define (open-output-bytevector)
  (open-output-string))

(define (get-output-bytevector p)
  (u8-list->bytevector
    (map char->integer (string->list (get-output-string p)))))

;;; R7RS write-shared must print shared structure with labels; s7's write
;;; does not, so both are s7's write (matching scheme/write.scm's choice).
(define write-simple write)
(define write-shared write)

;;; ---- environment / time -------------------------------------------

(define (command-line)
  (g_command-line))

(define (get-environment-variable key)
  (g_get-environment-variable key))

(define (get-environment-variables)
  (g_getenvs))

(define (environment . specs)
  (rootlet))

(define (interaction-environment)
  (rootlet))

(define (jiffies-per-second)
  1000000)

(define (current-second)
  (let-values (((sec usec) (g_get-time-of-day)))
    (+ sec (exact->inexact (/ usec 1000000)))))

(define (current-jiffy)
  (s7-round (* (current-second) (jiffies-per-second))))
