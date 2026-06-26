(define-library (liii base64)
  (import (scheme base) (liii base) (liii bitwise) (liii error))
  (export string-base64-encode
    bytevector-base64-encode
    base64-encode
    string-base64-decode
    bytevector-base64-decode
    base64-decode
  ) ;export
  (begin
    (define-constant BYTE2BASE64_BV
      (string->utf8 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
      ) ;string->utf8
    ) ;define-constant

    (define-constant BASE64_PAD_BYTE (char->integer #\=))

    (define bytevector-base64-encode
      (typed-lambda ((bv bytevector?))
        (define (encode b1 b2 b3)
          (let* ((p1 b1)
                 (p2 (if b2 b2 0))
                 (p3 (if b3 b3 0))
                 (combined (bitwise-ior (ash p1 16) (ash p2 8) p3))
                 (c1 (bitwise-and (ash combined -18) 63))
                 (c2 (bitwise-and (ash combined -12) 63))
                 (c3 (bitwise-and (ash combined -6) 63))
                 (c4 (bitwise-and combined 63))
                ) ;
            (values (BYTE2BASE64_BV c1)
              (BYTE2BASE64_BV c2)
              (if b2 (BYTE2BASE64_BV c3) BASE64_PAD_BYTE)
              (if b3 (BYTE2BASE64_BV c4) BASE64_PAD_BYTE)
            ) ;values
          ) ;let*
        ) ;define
        (let* ((input-N (bytevector-length bv))
               (output-N (* 4 (ceiling (/ input-N 3))))
               (output (make-bytevector output-N))
              ) ;
          (let loop
            ((i 0) (j 0))
            (when (< i input-N)
              (let* ((b1 (bv i))
                     (b2 (if (< (+ i 1) input-N) (bv (+ i 1)) #f))
                     (b3 (if (< (+ i 2) input-N) (bv (+ i 2)) #f))
                    ) ;
                (receive (r1 r2 r3 r4)
                  (encode b1 b2 b3)
                  (bytevector-u8-set! output j r1)
                  (bytevector-u8-set! output (+ j 1) r2)
                  (bytevector-u8-set! output (+ j 2) r3)
                  (bytevector-u8-set! output (+ j 3) r4)
                  (loop (+ i 3) (+ j 4))
                ) ;receive
              ) ;let*
            ) ;when
          ) ;let
          output
        ) ;let*
      ) ;typed-lambda
    ) ;define

    (define string-base64-encode
      (typed-lambda ((str string?))
        (utf8->string (bytevector-base64-encode (string->utf8 str)))
      ) ;typed-lambda
    ) ;define

    (define (base64-encode x)
      (cond ((string? x) (string-base64-encode x))
            ((bytevector? x) (bytevector-base64-encode x))
            (else (type-error "input must be string or bytevector"))
      ) ;cond
    ) ;define

    (define-constant BASE64_TO_BYTE_V
      (let ((byte2base64-N (bytevector-length BYTE2BASE64_BV)))
        (let loop
          ((i 0) (v (make-vector 256 -1)))
          (if (< i byte2base64-N)
            (begin
              (vector-set! v (BYTE2BASE64_BV i) i)
              (loop (+ i 1) v)
            ) ;begin
            v
          ) ;if
        ) ;let
      ) ;let
    ) ;define-constant

    (define (bytevector-base64-decode bv)
      (unless (bytevector? bv)
        (type-error "input must be bytevector")
      ) ;unless
      (let ((input-N (bytevector-length bv)))
        (unless (zero? (modulo input-N 4))
          (value-error "length of the input bytevector must be 4X")
        ) ;unless
        (define (decode c1 c2 c3 c4)
          (define pad? (lambda (c) (equal? c BASE64_PAD_BYTE)))
          (define c3-pad? (pad? c3))
          (define c4-pad? (pad? c4))
          (define b1 (BASE64_TO_BYTE_V c1))
          (define b2 (BASE64_TO_BYTE_V c2))
          (define b3 (BASE64_TO_BYTE_V c3))
          (define b4 (BASE64_TO_BYTE_V c4))
          (unless (and (not (pad? c1))
                    (not (pad? c2))
                    (not (negative? b1))
                    (not (negative? b2))
                    (or (and (not c3-pad?) (not c4-pad?) (not (negative? b3)) (not (negative? b4)))
                      (and (not c3-pad?) c4-pad? (not (negative? b3)))
                      (and c3-pad? c4-pad?)
                    ) ;or
                  ) ;and
            (value-error "Invalid base64 input")
          ) ;unless
          (values (bitwise-ior (ash b1 2) (ash b2 -4))
            (if c3-pad? 0 (bitwise-and (bitwise-ior (ash b2 4) (ash b3 -2)) 255))
            (if c4-pad? 0 (bitwise-and (bitwise-ior (ash b3 6) b4) 255))
            (if c3-pad? 1 (if c4-pad? 2 3))
          ) ;values
        ) ;define
        (let ((output-N (quotient (* input-N 3) 4)))
          (let ((output (make-bytevector output-N)))
            (let loop
              ((i 0) (j 0))
              (if (< i input-N)
                (receive (r1 r2 r3 cnt)
                  (decode (bv i) (bv (+ i 1)) (bv (+ i 2)) (bv (+ i 3)))
                  (bytevector-u8-set! output j r1)
                  (when (>= cnt 2)
                    (bytevector-u8-set! output (+ j 1) r2)
                  ) ;when
                  (when (>= cnt 3)
                    (bytevector-u8-set! output (+ j 2) r3)
                  ) ;when
                  (loop (+ i 4) (+ j cnt))
                ) ;receive
                (let ((final (make-bytevector j)))
                  (vector-copy! final 0 output 0 j)
                  final
                ) ;let
              ) ;if
            ) ;let
          ) ;let
        ) ;let
      ) ;let
    ) ;define

    (define string-base64-decode
      (typed-lambda ((str string?))
        (utf8->string (bytevector-base64-decode (string->utf8 str)))
      ) ;typed-lambda
    ) ;define

    (define (base64-decode x)
      (cond ((string? x) (string-base64-decode x))
            ((bytevector? x) (bytevector-base64-decode x))
            (else (type-error "input must be string or bytevector"))
      ) ;cond
    ) ;define
  ) ;begin
) ;define-library
