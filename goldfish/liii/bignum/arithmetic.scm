;;; liii/bignum/arithmetic.scm - bignum 算术运算与溢出升降级
;;;
;;; Copyright (c) 2026 Liii Network
;;; All Rights Reserved

(define-library (liii bignum arithmetic)
  (import (scheme base)
    (liii bignum native)
    (liii bigint)
  ) ;import

  (export INT64-MAX INT64-MIN INT64-MAX-BIG INT64-MIN-BIG exact-int? to-bigint
    demote-bigint bigint->inexact + - * / abs quotient remainder modulo gcd
    lcm expt floor ceiling truncate round numerator denominator
  ) ;export

  (begin
    (define INT64-MAX 9223372036854775807)
    (define INT64-MIN -9223372036854775808)
    (define INT64-MAX-BIG (string->bigint "9223372036854775807"))
    (define INT64-MIN-BIG (string->bigint "-9223372036854775808"))

    (define (exact-int? x)
      (or (bigint? x) (and (base:integer? x) (base:exact? x)))
    ) ;define

    (define (to-bigint x)
      (if (bigint? x) x (integer->bigint x))
    ) ;define

    (define (demote-bigint b)
      (if (and (bigint>=? b INT64-MIN-BIG) (bigint<=? b INT64-MAX-BIG))
        (base:string->number (bigint->string b))
        b
      ) ;if
    ) ;define

    (define (bigint->inexact b)
      (base:string->number (string-append (bigint->string b) ".0"))
    ) ;define

    (define (add-overflows? a b)
      (or (and (base:> b 0) (base:> a (base:- INT64-MAX b)))
        (and (base:< b 0) (base:< a (base:- INT64-MIN b)))
      ) ;or
    ) ;define

    (define (sub-overflows? a b)
      (or (and (base:< b 0) (base:> a (base:+ INT64-MAX b)))
        (and (base:> b 0) (base:< a (base:+ INT64-MIN b)))
      ) ;or
    ) ;define

    (define (mul-overflows? a b)
      (cond ((or (base:= a 0) (base:= b 0)) #f)
            ((or (base:= a INT64-MIN) (base:= b INT64-MIN))
             (not (or (base:= a 1) (base:= b 1)))
            ) ;
            (else (base:> (base:abs a) (base:quotient INT64-MAX (base:abs b))))
      ) ;cond
    ) ;define

    (define (add-2 a b)
      (cond ((and (base:integer? a) (base:exact? a) (base:integer? b) (base:exact? b))
             (if (add-overflows? a b)
               (demote-bigint (bigint-add (integer->bigint a) (integer->bigint b)))
               (base:+ a b)
             ) ;if
            )
            ((and (exact-int? a) (exact-int? b))
             (demote-bigint (bigint-add (to-bigint a) (to-bigint b)))
            )
            (else
             (base:+ (if (bigint? a) (bigint->inexact a) a)
               (if (bigint? b) (bigint->inexact b) b)
             ) ;base:+
            )
      ) ;cond
    ) ;define

    (define (negate-1 a)
      (cond ((bigint? a) (demote-bigint (bigint-negate a)))
            ((and (base:integer? a) (base:exact? a) (base:= a INT64-MIN))
             (bigint-negate INT64-MIN-BIG)
            ) ;
            (else (base:- a))
      ) ;cond
    ) ;define

    (define (sub-2 a b)
      (cond ((and (base:integer? a) (base:exact? a) (base:integer? b) (base:exact? b))
             (if (sub-overflows? a b)
               (demote-bigint (bigint-subtract (integer->bigint a) (integer->bigint b)))
               (base:- a b)
             ) ;if
            )
            ((and (exact-int? a) (exact-int? b))
             (demote-bigint (bigint-subtract (to-bigint a) (to-bigint b)))
            )
            (else
             (base:- (if (bigint? a) (bigint->inexact a) a)
               (if (bigint? b) (bigint->inexact b) b)
             ) ;base:-
            )
      ) ;cond
    ) ;define

    (define (mul-2 a b)
      (cond ((and (base:integer? a) (base:exact? a) (base:integer? b) (base:exact? b))
             (if (mul-overflows? a b)
               (demote-bigint (bigint-multiply (integer->bigint a) (integer->bigint b)))
               (base:* a b)
             ) ;if
            )
            ((and (exact-int? a) (exact-int? b))
             (demote-bigint (bigint-multiply (to-bigint a) (to-bigint b)))
            )
            (else
             (base:* (if (bigint? a) (bigint->inexact a) a)
               (if (bigint? b) (bigint->inexact b) b)
             ) ;base:*
            )
      ) ;cond
    ) ;define

    (define (div-2 a b)
      (cond ((and (base:integer? a) (base:exact? a) (base:integer? b) (base:exact? b))
             (if (and (base:= a INT64-MIN) (base:= b -1))
               (bigint-negate INT64-MIN-BIG)
               (base:/ a b)
             ) ;if
            )
            ((and (exact-int? a) (exact-int? b))
             (let ((ba (to-bigint a)) (bb (to-bigint b)))
               (when (bigint-zero? bb)
                 (base:/ 1 0)
               ) ;when
               (let-values (((q r) (bigint-divide-and-remainder ba bb)))
                 (if (bigint-zero? r)
                   (demote-bigint q)
                   (let* ((g (bigint-gcd (bigint-abs ba) (bigint-abs bb)))
                          (num (demote-bigint (bigint-quotient ba g)))
                          (den (demote-bigint (bigint-quotient bb g)))
                         ) ;
                     (if (and (base:integer? num) (base:integer? den))
                       (base:/ num den)
                       (base:/ (if (bigint? a) (bigint->inexact a) a)
                         (if (bigint? b) (bigint->inexact b) b)
                       ) ;base:/
                     ) ;if
                   ) ;let*
                 ) ;if
               ) ;let-values
             ) ;let
            )
            (else
             (base:/ (if (bigint? a) (bigint->inexact a) a)
               (if (bigint? b) (bigint->inexact b) b)
             ) ;base:/
            )
      ) ;cond
    ) ;define

    (define (+ . args)
      (if (null? args)
        0
        (let loop
          ((acc (car args)) (rest (cdr args)))
          (if (null? rest)
            acc
            (loop (add-2 acc (car rest)) (cdr rest))
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (- first . rest)
      (if (null? rest)
        (negate-1 first)
        (let loop
          ((acc first) (xs rest))
          (if (null? xs)
            acc
            (loop (sub-2 acc (car xs)) (cdr xs))
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (* . args)
      (if (null? args)
        1
        (let loop
          ((acc (car args)) (rest (cdr args)))
          (if (null? rest)
            acc
            (loop (mul-2 acc (car rest)) (cdr rest))
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (/ first . rest)
      (if (null? rest)
        (div-2 1 first)
        (let loop
          ((acc first) (xs rest))
          (if (null? xs)
            acc
            (loop (div-2 acc (car xs)) (cdr xs))
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (abs x)
      (cond ((bigint? x) (demote-bigint (bigint-abs x)))
            ((and (base:integer? x) (base:exact? x) (base:= x INT64-MIN))
             (bigint-negate INT64-MIN-BIG)
            ) ;
            (else (base:abs x))
      ) ;cond
    ) ;define

    (define (quotient a b)
      (if (or (bigint? a) (bigint? b))
        (if (and (exact-int? a) (exact-int? b))
          (demote-bigint (bigint-quotient (to-bigint a) (to-bigint b)))
          (base:quotient (if (bigint? a) (bigint->inexact a) a)
            (if (bigint? b) (bigint->inexact b) b)
          ) ;base:quotient
        ) ;if
        (if (and (base:integer? a) (base:exact? a) (base:integer? b) (base:exact? b)
                 (base:= a INT64-MIN) (base:= b -1))
          (bigint-negate INT64-MIN-BIG)
          (base:quotient a b)
        ) ;if
      ) ;if
    ) ;define

    (define (remainder a b)
      (if (or (bigint? a) (bigint? b))
        (if (and (exact-int? a) (exact-int? b))
          (demote-bigint (bigint-remainder (to-bigint a) (to-bigint b)))
          (base:remainder (if (bigint? a) (bigint->inexact a) a)
            (if (bigint? b) (bigint->inexact b) b)
          ) ;base:remainder
        ) ;if
        (base:remainder a b)
      ) ;if
    ) ;define

    (define (modulo a b)
      (if (or (bigint? a) (bigint? b))
        (if (and (exact-int? a) (exact-int? b))
          (demote-bigint (bigint-modulo (to-bigint a) (to-bigint b)))
          (base:modulo (if (bigint? a) (bigint->inexact a) a)
            (if (bigint? b) (bigint->inexact b) b)
          ) ;base:modulo
        ) ;if
        (base:modulo a b)
      ) ;if
    ) ;define

    (define (gcd . args)
      (if (null? args)
        0
        (let loop
          ((acc (abs (car args))) (xs (cdr args)))
          (if (null? xs)
            acc
            (let ((y (abs (car xs))))
              (if (and (exact-int? acc) (exact-int? y))
                (loop (demote-bigint (bigint-gcd (to-bigint acc) (to-bigint y))) (cdr xs))
                (loop (base:gcd (if (bigint? acc) (bigint->inexact acc) acc)
                        (if (bigint? y) (bigint->inexact y) y)
                      ) ;base:gcd
                  (cdr xs)
                ) ;loop
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (lcm . args)
      (if (null? args)
        1
        (let loop
          ((acc (abs (car args))) (xs (cdr args)))
          (if (null? xs)
            acc
            (let ((y (abs (car xs))))
              (cond ((or (and (bigint? acc) (bigint-zero? acc)) (and (not (bigint? acc)) (base:zero? acc))
                         (and (bigint? y) (bigint-zero? y)) (and (not (bigint? y)) (base:zero? y)))
                     (loop 0 (cdr xs)))
                    ((and (exact-int? acc) (exact-int? y))
                     (let* ((ba (to-bigint acc))
                            (bb (to-bigint y))
                            (g (bigint-gcd ba bb))
                           ) ;
                       (loop (demote-bigint (bigint-multiply (bigint-quotient ba g) bb))
                         (cdr xs)
                       ) ;loop
                     ) ;let*
                    )
                    (else
                     (loop (base:lcm (if (bigint? acc) (bigint->inexact acc) acc)
                             (if (bigint? y) (bigint->inexact y) y)
                           ) ;base:lcm
                       (cdr xs)
                     ) ;loop
                    )
              ) ;cond
            ) ;let
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (expt base exp)
      (cond ((and (exact-int? base) (exact-int? exp) (not (if (bigint? exp) (bigint-negative? exp) (base:negative? exp))))
             (if (and (base:integer? exp) (base:exact? exp))
               (demote-bigint (bigint-expt (to-bigint base) exp))
               (if (if (bigint? base) (bigint-zero? base) (base:zero? base))
                 0
                 (if (and (not (bigint? base)) (base:= base 1))
                   1
                   (base:expt (if (bigint? base) (bigint->inexact base) (base:inexact base))
                     (if (bigint? exp) (bigint->inexact exp) (base:inexact exp))
                   ) ;base:expt
                 ) ;if
               ) ;if
             ) ;if
            )
            ((and (exact-int? base) (exact-int? exp))
             (div-2 1 (expt base (negate-1 exp)))
            )
            (else
             (base:expt (if (bigint? base) (bigint->inexact base) base)
               (if (bigint? exp) (bigint->inexact exp) exp)
             ) ;base:expt
            )
      ) ;cond
    ) ;define

    (define (floor x) (if (bigint? x) x (base:floor x)))
    (define (ceiling x) (if (bigint? x) x (base:ceiling x)))
    (define (truncate x) (if (bigint? x) x (base:truncate x)))
    (define (round x) (if (bigint? x) x (base:round x)))
    (define (numerator x) (if (bigint? x) x (base:numerator x)))
    (define (denominator x) (if (bigint? x) 1 (base:denominator x)))
  ) ;begin
) ;define-library
