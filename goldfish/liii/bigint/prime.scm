;;; liii/bigint/prime.scm - bigint 素数检测与随机数生成
;;;
;;; Copyright (c) 2024-2026 Liii Network
;;; All Rights Reserved

(define-library (liii bigint prime)
  (import (scheme base)
    (liii error)
    (liii bigint record)
    (liii bigint constant)
    (liii bigint compare)
    (liii bigint predicate)
    (liii bigint string)
    (liii bigint arithmetic)
    (liii bigint math)
    (liii bigint bitwise)
  ) ;import

  (export make-random-bigint make-random-bigint-below bigint-prime?
    bigint-next-prime
  ) ;export

  (begin
    (define (normalize-certainty certainty)
      (if (not (integer? certainty))
        (type-error "bigint-prime? in (liii bigint prime): certainty must be an integer"
          certainty
        ) ;type-error
        (if (< certainty 0)
          (value-error "bigint-prime? in (liii bigint prime): certainty must be non-negative"
            certainty
          ) ;value-error
          certainty
        ) ;if
      ) ;if
    ) ;define

    (define (make-random-bigint num-bits)
      (if (not (integer? num-bits))
        (type-error "make-random-bigint in (liii bigint prime): num-bits must be an integer"
          num-bits
        ) ;type-error
        (if (<= num-bits 0)
          (value-error "make-random-bigint in (liii bigint prime): num-bits must be positive"
            num-bits
          ) ;value-error
          (let loop
            ((remaining num-bits) (acc bigint-zero))
            (if (= remaining 0)
              acc
              (let ((bit
                      (if (= remaining num-bits)
                        bigint-one
                        (if (= (random 2) 0) bigint-zero bigint-one)
                      ) ;if
                    ) ;bit
                   ) ;
                (loop (- remaining 1) (bigint-add (bigint-multiply acc bigint-two) bit))
              ) ;let
            ) ;if
          ) ;let
        ) ;if
      ) ;if
    ) ;define

    (define (make-random-bigint-below bound)
      (let ((value (ensure-bigint "make-random-bigint-below" bound)))
        (if (bigint<=? value bigint-zero)
          bigint-zero
          (let ((bits (bigint-bit-length value)))
            (let loop
              ()
              (let bit-loop
                ((remaining bits) (acc bigint-zero))
                (if (= remaining 0)
                  (if (bigint<? acc value) acc (loop))
                  (let ((bit (if (= (random 2) 0) bigint-zero bigint-one)))
                    (bit-loop (- remaining 1) (bigint-add (bigint-multiply acc bigint-two) bit))
                  ) ;let
                ) ;if
              ) ;let
            ) ;let
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (small-prime-composite? n)
      (let loop
        ((primes '(2 3 5 7 11 13 17 19 23 29 31 37)))
        (if (null? primes)
          #f
          (let ((prime (integer->bigint (car primes))))
            (cond ((bigint=? n prime) 'prime)
                  ((bigint-zero? (bigint-remainder n prime)) #t)
                  (else (loop (cdr primes)))
            ) ;cond
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (decompose-factor-of-two n)
      (let loop
        ((s 0) (d n))
        (if (bigint-even? d) (loop (+ s 1) (bigint-divide d bigint-two)) (values s d))
      ) ;let
    ) ;define

    (define (miller-rabin-witness? a n s d)
      (let ((n-minus-one (bigint-subtract n bigint-one)))
        (let loop
          ((r 0) (x (bigint-mod-expt a d n)))
          (cond ((bigint=? x bigint-one) #f)
                ((bigint=? x n-minus-one) #f)
                ((= r (- s 1)) #t)
                (else (loop (+ r 1) (bigint-mod (bigint-multiply x x) n)))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    (define (miller-rabin-test value rounds s d)
      (let ((witness-bound (bigint-subtract value (integer->bigint 3))))
        (let loop
          ((remaining rounds))
          (if (= remaining 0)
            #t
            (let ((a (bigint-add bigint-two (make-random-bigint-below witness-bound))))
              (if (miller-rabin-witness? a value s d) #f (loop (- remaining 1)))
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (bigint-prime? n . maybe-certainty)
      (let* ((value (ensure-bigint "bigint-prime?" n))
             (certainty (if (null? maybe-certainty) 10 (normalize-certainty (car maybe-certainty)))
             ) ;certainty
             (rounds (if (= certainty 0) 1 certainty))
            ) ;
        (cond ((bigint<=? value bigint-one) #f)
              ((bigint=? value bigint-two) #t)
              ((bigint-even? value) #f)
              (else
                (let ((small-prime-result (small-prime-composite? value)))
                  (cond ((eq? small-prime-result 'prime) #t)
                        (small-prime-result #f)
                        (else
                          (call-with-values (lambda () (decompose-factor-of-two (bigint-subtract value bigint-one)))
                            (lambda (s d) (miller-rabin-test value rounds s d))
                          ) ;call-with-values
                        ) ;else
                  ) ;cond
                ) ;let
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    (define (bigint-next-prime n)
      (let ((value (ensure-bigint "bigint-next-prime" n)))
        (if (bigint<? value bigint-two)
          bigint-two
          (let ((start (bigint-add value bigint-one)))
            (let ((candidate (if (bigint-even? start) (bigint-add start bigint-one) start)))
              (let loop
                ((candidate candidate))
                (if (bigint-prime? candidate)
                  candidate
                  (loop (bigint-add candidate bigint-two))
                ) ;if
              ) ;let
            ) ;let
          ) ;let
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
