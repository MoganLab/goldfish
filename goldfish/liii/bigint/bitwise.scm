;;; liii/bigint/bitwise.scm - bigint 位运算实现
;;;
;;; Copyright (c) 2024-2026 Liii Network
;;; All Rights Reserved

(define-library (liii bigint bitwise)
  (import (scheme base)
    (liii error)
    (liii bitwise)
    (liii bigint record)
    (liii bigint constant)
    (liii bigint predicate)
    (liii bigint conversion)
    (liii bigint arithmetic)
  ) ;import

  (export bigint-and bigint-or bigint-xor bigint-not bigint-arithmetic-shift
    bigint-bit-set? bigint-bit-set bigint-bit-clear bigint-bit-flip
    bigint-bit-length bigint-bit-count bigint-lowest-bit-set
  ) ;export

  (begin
    (define (bigint-bit-ref-simple n bit-index)
      (let* ((shifted (bigint-arithmetic-shift n (- bit-index)))
             (bit (bigint-odd? shifted))
            ) ;
        (if bit 1 0)
      ) ;let*
    ) ;define

    (define (bytevector->u8-list bytes)
      (let loop
        ((index 0) (result '()))
        (if (= index (bytevector-length bytes))
          (reverse result)
          (loop (+ index 1) (cons (bytevector-u8-ref bytes index) result))
        ) ;if
      ) ;let
    ) ;define

    (define (u8-list->bytevector bytes)
      (let ((result (make-bytevector (length bytes) 0)))
        (let loop
          ((index 0) (rest bytes))
          (if (null? rest)
            result
            (begin
              (bytevector-u8-set! result index (car rest))
              (loop (+ index 1) (cdr rest))
            ) ;begin
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (sign-extension-byte n)
      (if (bigint-negative? n) 255 0)
    ) ;define

    (define (extend-bytes bytes target-len fill-byte)
      (if (>= (length bytes) target-len)
        bytes
        (append (make-list (- target-len (length bytes)) fill-byte) bytes)
      ) ;if
    ) ;define

    (define (bytewise-op a b op-name op-func)
      (let* ((left (ensure-bigint op-name a))
             (right (ensure-bigint op-name b))
             (left-bytes (bytevector->u8-list (bigint->bytes left)))
             (right-bytes (bytevector->u8-list (bigint->bytes right)))
             (target-len (max (length left-bytes) (length right-bytes)))
             (extended-left (extend-bytes left-bytes target-len (sign-extension-byte left)))
             (extended-right (extend-bytes right-bytes target-len (sign-extension-byte right))
             ) ;extended-right
             (result-bytes (map op-func extended-left extended-right))
            ) ;
        (bytes->bigint (u8-list->bytevector result-bytes))
      ) ;let*
    ) ;define

    (define (digit-popcount d)
      (let loop
        ((n d) (count 0))
        (if (= n 0) count (loop (quotient n 2) (+ count (remainder n 2))))
      ) ;let
    ) ;define

    (define (mag-popcount mag)
      (let loop
        ((digits mag) (count 0))
        (if (null? digits)
          count
          (loop (cdr digits) (+ count (digit-popcount (car digits))))
        ) ;if
      ) ;let
    ) ;define

    (define (mag-bit-length mag)
      (if (null? mag)
        0
        (let* ((msb (car mag))
               (high-bits (length mag))
               (bits-in-msb
                 (let loop
                   ((n msb) (bits 0))
                   (if (= n 0) bits (loop (quotient n 2) (+ bits 1)))
                 ) ;let
               ) ;bits-in-msb
              ) ;
          (+ (* (- high-bits 1) 32) bits-in-msb)
        ) ;let*
      ) ;if
    ) ;define

    (define (mag-power-of-two? mag)
      (= (mag-popcount mag) 1)
    ) ;define

    ;;; ---------- 位运算 ----------

    (define (bigint-and a b)
      (bytewise-op a b "bigint-and" bitwise-and)
    ) ;define

    (define (bigint-or a b)
      (bytewise-op a b "bigint-or" bitwise-ior)
    ) ;define

    (define (bigint-xor a b)
      (bytewise-op a b "bigint-xor" bitwise-xor)
    ) ;define

    (define (bigint-not n)
      (let ((value (ensure-bigint "bigint-not" n)))
        (bigint-subtract (bigint-negate value) (integer->bigint 1))
      ) ;let
    ) ;define

    (define (bigint-arithmetic-shift a n)
      (let ((value (ensure-bigint "bigint-arithmetic-shift" a)))
        (cond ((= n 0) value)
              ((> n 0) (bigint-multiply value (bigint-expt (integer->bigint 2) n)))
              (else
                (let ((divisor (bigint-expt (integer->bigint 2) (- n))))
                  (call-with-values (lambda () (bigint-divide-and-remainder value divisor))
                    (lambda (quotient remainder)
                      (if (and (bigint-negative? value) (not (bigint-zero? remainder)))
                        (bigint-subtract quotient (integer->bigint 1))
                        quotient
                      ) ;if
                    ) ;lambda
                  ) ;call-with-values
                ) ;let
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    ;;; ---------- 位测试与操作 ----------

    (define (bigint-bit-set? a n)
      (let ((value (ensure-bigint "bigint-bit-set?" a)))
        (if (< n 0)
          (value-error "bigint-bit-set? in (liii bigint bitwise): bit index must be non-negative"
            n
          ) ;value-error
          (= 1 (bigint-bit-ref-simple value n))
        ) ;if
      ) ;let
    ) ;define

    (define (bigint-bit-set a n)
      (let ((value (ensure-bigint "bigint-bit-set" a)))
        (if (< n 0)
          (value-error "bigint-bit-set in (liii bigint bitwise): bit index must be non-negative"
            n
          ) ;value-error
          (bigint-or value (bigint-arithmetic-shift (integer->bigint 1) n))
        ) ;if
      ) ;let
    ) ;define

    (define (bigint-bit-clear a n)
      (let ((value (ensure-bigint "bigint-bit-clear" a)))
        (if (< n 0)
          (value-error "bigint-bit-clear in (liii bigint bitwise): bit index must be non-negative"
            n
          ) ;value-error
          (if (bigint-negative? value)
            (bigint-and value (bigint-not (bigint-arithmetic-shift (integer->bigint 1) n)))
            (if (bigint-bit-set? value n)
              (bigint-subtract value (bigint-arithmetic-shift (integer->bigint 1) n))
              value
            ) ;if
          ) ;if
        ) ;if
      ) ;let
    ) ;define

    (define (bigint-bit-flip a n)
      (let ((value (ensure-bigint "bigint-bit-flip" a)))
        (if (< n 0)
          (value-error "bigint-bit-flip in (liii bigint bitwise): bit index must be non-negative"
            n
          ) ;value-error
          (bigint-xor value (bigint-arithmetic-shift (integer->bigint 1) n))
        ) ;if
      ) ;let
    ) ;define

    ;;; ---------- 位查询 ----------

    (define (bigint-bit-length a)
      (let ((value (ensure-bigint "bigint-bit-length" a)))
        (let ((mag (bigint-mag value)))
          (if (null? mag)
            0
            (let ((length (mag-bit-length mag)))
              (if (and (bigint-negative? value) (mag-power-of-two? mag)) (- length 1) length)
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (bigint-bit-count a)
      (let ((value (ensure-bigint "bigint-bit-count" a)))
        (cond ((bigint-zero? value) 0)
              ((bigint-negative? value)
               (mag-popcount (bigint-mag (bigint-subtract (bigint-abs value) (integer->bigint 1)))
               ) ;mag-popcount
              ) ;
              (else (mag-popcount (bigint-mag value)))
        ) ;cond
      ) ;let
    ) ;define

    (define (bigint-lowest-bit-set a)
      (let ((value (ensure-bigint "bigint-lowest-bit-set" a)))
        (let ((mag (bigint-mag value)))
          (if (null? mag)
            -1
            (let loop
              ((digits (reverse mag)) (idx 0))
              (if (null? digits)
                -1
                (let ((d (car digits)))
                  (if (= d 0)
                    (loop (cdr digits) (+ idx 32))
                    (let bit-loop
                      ((n d) (bit 0))
                      (if (odd? n) (+ idx bit) (bit-loop (quotient n 2) (+ bit 1)))
                    ) ;let
                  ) ;if
                ) ;let
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
