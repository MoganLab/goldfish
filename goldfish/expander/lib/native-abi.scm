;;; Scheme implementations for R7RS values not supplied by the native
;;; runtime. Loaded by the native driver before user libraries.

;;; ---- promises -------------------------------------------------------

(define (promise? x)
  (and (pair? x) (pair? (cdr x)) (eq? (cadr x) '+promise+)))

(define (make-promise obj)
  (if (promise? obj) obj (make-lazy-promise (lambda () obj))))

;;; ---- ports ----------------------------------------------------------

(define (port? p) (or (input-port? p) (output-port? p)))

(define (close-port p)
  (if (input-port? p) (close-input-port p) (close-output-port p)))

(define (call-with-port port proc)
  (let ((res (proc port)))
    (if res (close-port port))
    res))

;; goldfish does not distinguish textual/binary ports; binary I/O rides
;; on char-based ports (a byte is a character with code point 0-255).
(define (binary-port? p) (port? p))
(define (textual-port? p) (port? p))

(define (input-port-open? p) (not (port-closed? p)))
(define (output-port-open? p) (not (port-closed? p)))

(define open-binary-input-file open-input-file)
(define open-binary-output-file open-output-file)

;; s7's eval-string surface is also used by (liii base).  Keep the
;; implementation in Scheme and evaluate each datum through the native
;; evaluator, returning the last result.
(define (eval-string source . maybe-environment)
  (if (not (string? source))
    (error 'wrong-type-arg "eval-string: expected a string" source)
    (if (or (null? maybe-environment) (null? (cdr maybe-environment)))
      (let ((port (open-input-string source)))
        (let loop ((result #f))
          (let ((form (read port)))
            (if (eof-object? form)
              (begin (close-input-port port) result)
              (loop (if (null? maybe-environment)
                      (eval form)
                      (eval form (car maybe-environment))))))))
      (error 'wrong-number-of-args "eval-string: expected one or two arguments")))
)

;; Error predicates over the pair format the tests (and legacy handlers)
;; use; non-pairs are simply not errors.
(define (read-error? obj) (and (pair? obj) (eq? (car obj) 'read-error)))
(define (file-error? obj) (and (pair? obj) (eq? (car obj) 'io-error)))

;;; ---- binary I/O -----------------------------------------------------

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
  ;; R7RS: (write-bytevector bv [port [start [end]]])
  (let* ((port (if (and (pair? rest) (output-port? (car rest)))
                 (car rest)
                 (current-output-port)))
         (range (if (and (pair? rest) (output-port? (car rest)))
                  (cdr rest)
                  rest))
         (start (if (and (pair? range) (integer? (car range))) (car range) 0))
         (end (if (and (pair? range) (integer? (car range))
                       (pair? (cdr range)) (integer? (cadr range)))
                (cadr range)
                (bytevector-length bv))))
    (let loop ((i start))
      (unless (= i end)
        (write-u8 (bytevector-u8-ref bv i) port)
        (loop (+ i 1))))))

(define (read-bytevector! bv . rest)
  ;; R7RS: (read-bytevector! bv [port [start [end]]])
  (let* ((port (if (and (pair? rest) (input-port? (car rest)))
                 (car rest)
                 (current-input-port)))
         (range (if (and (pair? rest) (input-port? (car rest)))
                  (cdr rest)
                  rest))
         (start (if (and (pair? range) (integer? (car range))) (car range) 0))
         (end (if (and (pair? range) (integer? (car range))
                       (pair? (cdr range)) (integer? (cadr range)))
                (cadr range)
                (bytevector-length bv))))
    (let loop ((i start))
      (if (< i end)
        (let ((b (read-u8 port)))
          (if (eof-object? b)
            (- i start)
            (begin (bytevector-u8-set! bv i b)
                   (loop (+ i 1)))))
        (- end start)))))

(define (open-input-bytevector bv)
  (g-open-input-bytevector bv))

(define (open-output-bytevector)
  (open-output-string))

(define (get-output-bytevector p)
  (u8-list->bytevector
    (map char->integer (string->list (get-output-string p)))))

;;; R7RS write-shared/write-simple: native already provides both
;;; (standard_primitives), so nothing to alias here.

;;; ---- bytevector operations ------------------------------------------

(define (bytevector-length x)
  (cond
    ((bytevector? x) (length (bytevector->u8-list x)))
    ((string? x) (string-length x))
    ((vector? x) (vector-length x))
    ((list? x) (length x))
    (else #f)))

(define (bytevector-copy v . maybe-range)
  (let* ((start (if (pair? maybe-range) (car maybe-range) 0))
         (end (if (and (pair? maybe-range) (pair? (cdr maybe-range)))
                (cadr maybe-range)
                (bytevector-length v))))
    (if (or (< start 0) (> start end) (> end (bytevector-length v)))
      (error 'out-of-range "bytevector-copy"))
    (let ((new-v (make-bytevector (- end start))))
      (let loop ((i start) (j 0))
        (if (>= i end)
          new-v
          (begin
            (bytevector-u8-set! new-v j (bytevector-u8-ref v i))
            (loop (+ i 1) (+ j 1))))))))

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

;;; ---- UTF-8 operations ------------------------------------------------

(define (bytevector-advance-utf8 bv index . maybe-end)
  ;; Index after the UTF-8 sequence starting at index; stays put on
  ;; truncated/invalid sequences. end defaults to
  ;; the bytevector length (define* parity).
  (let ((end (if (pair? maybe-end) (car maybe-end) (bytevector-length bv))))
    (if (>= index end)
      index
      (let ((byte (bytevector-u8-ref bv index)))
        (cond
          ((< byte 128) (+ index 1))
          ((< byte 224)
           (if (>= (+ index 1) end)
             index
             (let ((next-byte (bytevector-u8-ref bv (+ index 1))))
               (if (not (= (logand next-byte 192) 128))
                 index
                 (+ index 2)))))
          ((< byte 240)
           (if (>= (+ index 2) end)
             index
             (let ((nb1 (bytevector-u8-ref bv (+ index 1)))
                   (nb2 (bytevector-u8-ref bv (+ index 2))))
               (if (or (not (= (logand nb1 192) 128))
                       (not (= (logand nb2 192) 128)))
                 index
                 (+ index 3)))))
          ((< byte 248)
           (if (>= (+ index 3) end)
             index
             (let ((nb1 (bytevector-u8-ref bv (+ index 1)))
                   (nb2 (bytevector-u8-ref bv (+ index 2)))
                   (nb3 (bytevector-u8-ref bv (+ index 3))))
               (if (or (not (= (logand nb1 192) 128))
                       (not (= (logand nb2 192) 128))
                       (not (= (logand nb3 192) 128)))
                 index
                 (+ index 4)))))
          (else index))))))

(define (utf8-codepoint-at bv pos)
  (let ((b0 (bytevector-u8-ref bv pos)))
    (cond
      ((<= b0 127) b0)
      ((<= b0 223)
       (logior (ash (logand b0 31) 6)
               (logand (bytevector-u8-ref bv (+ pos 1)) 63)))
      ((<= b0 239)
       (logior (ash (logand b0 15) 12)
               (ash (logand (bytevector-u8-ref bv (+ pos 1)) 63) 6)
               (logand (bytevector-u8-ref bv (+ pos 2)) 63)))
      (else
       (logior (ash (logand b0 7) 18)
               (ash (logand (bytevector-u8-ref bv (+ pos 1)) 63) 12)
               (ash (logand (bytevector-u8-ref bv (+ pos 2)) 63) 6)
               (logand (bytevector-u8-ref bv (+ pos 3)) 63))))))

(define (codepoint->utf8-bytes cp)
  (cond
    ((<= cp 127) (list cp))
    ((<= cp 2047)
     (list (logior 192 (ash cp -6)) (logior 128 (logand cp 63))))
    ((<= cp 65535)
     (list (logior 224 (ash cp -12))
           (logior 128 (ash (logand (ash cp -6) 63) 0))
           (logior 128 (logand cp 63))))
    (else
     (list (logior 240 (ash cp -18))
           (logior 128 (logand (ash cp -12) 63))
           (logior 128 (logand (ash cp -6) 63))
           (logior 128 (logand cp 63))))))

(define (utf8-string->chars str)
  (let ((bv (string->utf8 str))
        (len (string-length str)))
    (let loop ((pos 0) (acc '()))
      (if (>= pos len)
        (reverse acc)
        (let ((next (bytevector-advance-utf8 bv pos len)))
          (if (= next pos)
            (error 'value-error "invalid UTF-8 sequence at index: " pos)
            (loop next (cons (integer->char (utf8-codepoint-at bv pos))
                             acc))))))))

(define (utf8-chars->string ls)
  (let* ((bls (apply append (map codepoint->utf8-bytes (map char->integer ls))))
         (bv (make-bytevector (length bls))))
    (let loop ((i 0) (b bls))
      (if (null? b)
        (utf8->string bv)
        (begin
          (bytevector-u8-set! bv i (car b))
          (loop (+ i 1) (cdr b)))))))

(define (utf8-string-length str)
  (if (not (string? str))
    (error 'wrong-type-arg "utf8-string-length expects a string" str)
    (let ((bv (string->utf8 str))
          (n (string-length str)))
      (if (zero? n)
        0
        (let loop ((pos 0) (cnt 0))
          (let ((next-pos (bytevector-advance-utf8 bv pos n)))
            (cond
              ((= next-pos n) (+ cnt 1))
              ((= next-pos pos)
               (error 'value-error "Invalid UTF-8 sequence at index: " pos))
              (else (loop next-pos (+ cnt 1))))))))))

;;; ---- string/vector conversions and friends --------------------------

(define (string->vector . args)
  ;; Zero-arg must raise 'wrong-type-arg (host define* parity).  Any
  ;; sequence source works (host uses s7's generic copy: the tests pass
  ;; strings, lists and vectors); non-sequences fail in string-ref.
  (when (null? args)
    (error 'wrong-type-arg "string->vector: string required"))
  (let* ((s (car args))
         (maybe-range (cdr args))
         (start (if (pair? maybe-range) (car maybe-range) 0))
         (end (if (and (pair? maybe-range) (pair? (cdr maybe-range)))
                (cadr maybe-range)
                (cond ((string? s) (string-length s))
                      ((vector? s) (vector-length s))
                      (else (length s)))))
         (ref (cond ((string? s) string-ref)
                    ((vector? s) vector-ref)
                    ((list? s) list-ref)
                    (else (lambda (seq i) (error 'type-error "string->vector: expected sequence" seq)))))
         (vec (make-vector (- end start))))
    (let loop ((i start) (j 0))
      (if (>= i end)
        vec
        (begin
          (vector-set! vec j (ref s i))
          (loop (+ i 1) (+ j 1)))))))

(define (vector->string . args)
  ;; Same zero-arg key contract; generic sequence source like the host.
  (when (null? args)
    (error 'wrong-type-arg "vector->string: vector required"))
  (let* ((v (car args))
         (maybe-range (cdr args))
         (start (if (pair? maybe-range) (car maybe-range) 0))
         (end (if (and (pair? maybe-range) (pair? (cdr maybe-range)))
                (cadr maybe-range)
                (cond ((vector? v) (vector-length v))
                      ((string? v) (string-length v))
                      (else (length v)))))
         (ref (cond ((vector? v) vector-ref)
                    ((string? v) string-ref)
                    ((list? v) list-ref)
                    (else (lambda (seq i) (error 'type-error "vector->string: expected sequence" seq)))))
         (str (make-string (- end start))))
    (let loop ((i start) (j 0))
      (if (>= i end)
        str
        (let ((ch (ref v i)))
          (when (not (char? ch))
            ;; Tests pin 'wrong-type-arg for non-character elements (the
            ;; message classifier would call it type-error).
            (error 'wrong-type-arg "vector->string: expected character"))
          (string-set! str j ch)
          (loop (+ i 1) (+ j 1)))))))

(define (symbol=? sym1 sym2 . rest)
  ;; Host-abi body verbatim: non-symbol => #f, pairwise eq?, rest all
  ;; equal to sym1.
  (define (same-symbol sym rest)
    (if (null? rest)
      #t
      (and (eq? sym (car rest)) (same-symbol sym (cdr rest)))))
  (cond
    ((not (symbol? sym1)) #f)
    ((not (symbol? sym2)) #f)
    ((not (eq? sym1 sym2)) #f)
    (else (same-symbol sym1 rest))))

(define (string-map p . args)
  (if (not (procedure? p))
    (error 'wrong-type-arg "string-map: procedure expected" p)
    (if (null? args)
      ""
      (utf8-chars->string (apply map p (map utf8-string->chars args))))))

(define (string-for-each p . args)
  (if (not (procedure? p))
    (error 'wrong-type-arg "string-for-each: procedure expected" p)
    (let check-strings ((rest args))
      (cond ((null? rest) (apply for-each p (map utf8-string->chars args)))
            ((not (string? (car rest)))
             (error 'wrong-type-arg "string-for-each: expected string" (car rest)))
            (else (check-strings (cdr rest)))))))

;;; ---- misc -----------------------------------------------------------

;; s7 compatibility: its lcm is R7RS lcm for the exact arguments the
;; s7-lcm test pins.
(define (s7-lcm . args) (apply lcm args))

;; Host s7 seeds this from its own feature list; only claim what the
;; native runtime actually is (ieee-float/ratios/complex join the float
;; workstream).
(define *features* '(goldfish linux r7rs))
