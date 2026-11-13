;;; liii/bignum/arithmetic.scm - bignum 算术运算与溢出升降级（由 liii/bignum.scm include）
;;;
;;; Copyright (c) 2026 Liii Network
;;; All Rights Reserved

(define (bn:add-overflows? a b)
  (or (and (native:> b 0) (native:> a (native:- INT64-MAX b)))
    (and (native:< b 0) (native:< a (native:- INT64-MIN b)))
  ) ;or
) ;define

(define (bn:sub-overflows? a b)
  (or (and (native:< b 0) (native:> a (native:+ INT64-MAX b)))
    (and (native:> b 0) (native:< a (native:+ INT64-MIN b)))
  ) ;or
) ;define

(define (bn:mul-overflows? a b)
  (cond ((or (native:= a 0) (native:= b 0)) #f)
        ((or (native:= a INT64-MIN) (native:= b INT64-MIN))
         (not (or (native:= a 1) (native:= b 1)))
        ) ;
        (else (native:> (native:abs a) (native:quotient INT64-MAX (native:abs b))))
  ) ;cond
) ;define

(define (bn:add-2 a b)
  (cond
   ((and (native:integer? a)
      (native:exact? a)
      (native:integer? b)
      (native:exact? b)
    ) ;and
    (if (bn:add-overflows? a b)
      (bn:demote-bigint (bigint-add (integer->bigint a) (integer->bigint b)))
      (native:+ a b)
    ) ;if
   ) ;
   ((and (bn:exact-int? a) (bn:exact-int? b))
    (bn:demote-bigint (bigint-add (bn:to-bigint a) (bn:to-bigint b)))
   ) ;
   (else (native:+ (if (bigint? a) (bn:bigint->inexact a) a)
           (if (bigint? b) (bn:bigint->inexact b) b)
         ) ;native:+
   ) ;else
  ) ;cond
) ;define

(define (bn:negate-1 a)
  (cond ((bigint? a) (bn:demote-bigint (bigint-negate a)))
        ((and (native:integer? a) (native:exact? a) (native:= a INT64-MIN))
         (bigint-negate INT64-MIN-BIG)
        ) ;
        (else (native:- a))
  ) ;cond
) ;define

(define (bn:sub-2 a b)
  (cond
   ((and (native:integer? a)
      (native:exact? a)
      (native:integer? b)
      (native:exact? b)
    ) ;and
    (if (bn:sub-overflows? a b)
      (bn:demote-bigint (bigint-subtract (integer->bigint a) (integer->bigint b)))
      (native:- a b)
    ) ;if
   ) ;
   ((and (bn:exact-int? a) (bn:exact-int? b))
    (bn:demote-bigint (bigint-subtract (bn:to-bigint a) (bn:to-bigint b)))
   ) ;
   (else (native:- (if (bigint? a) (bn:bigint->inexact a) a)
           (if (bigint? b) (bn:bigint->inexact b) b)
         ) ;native:-
   ) ;else
  ) ;cond
) ;define

(define (bn:mul-2 a b)
  (cond
   ((and (native:integer? a)
      (native:exact? a)
      (native:integer? b)
      (native:exact? b)
    ) ;and
    (if (bn:mul-overflows? a b)
      (bn:demote-bigint (bigint-multiply (integer->bigint a) (integer->bigint b)))
      (native:* a b)
    ) ;if
   ) ;
   ((and (bn:exact-int? a) (bn:exact-int? b))
    (bn:demote-bigint (bigint-multiply (bn:to-bigint a) (bn:to-bigint b)))
   ) ;
   (else (native:* (if (bigint? a) (bn:bigint->inexact a) a)
           (if (bigint? b) (bn:bigint->inexact b) b)
         ) ;native:*
   ) ;else
  ) ;cond
) ;define

(define (bn:div-2 a b)
  (cond
   ((and (native:integer? a)
      (native:exact? a)
      (native:integer? b)
      (native:exact? b)
    ) ;and
    (if (and (native:= a INT64-MIN) (native:= b -1))
      (bigint-negate INT64-MIN-BIG)
      (native:/ a b)
    ) ;if
   ) ;
   ((and (bn:exact-int? a) (bn:exact-int? b))
    (let ((ba (bn:to-bigint a)) (bb (bn:to-bigint b)))
      (when (bigint-zero? bb)
        (native:/ 1 0)
      ) ;when
      (let-values (((q r) (bigint-divide-and-remainder ba bb)))
        (if (bigint-zero? r)
          (bn:demote-bigint q)
          (let* ((g (bigint-gcd (bigint-abs ba) (bigint-abs bb)))
                 (num (bn:demote-bigint (bigint-quotient ba g)))
                 (den (bn:demote-bigint (bigint-quotient bb g)))
                ) ;
            (if (and (native:integer? num) (native:integer? den))
              (native:/ num den)
              (native:/ (if (bigint? a) (bn:bigint->inexact a) a)
                (if (bigint? b) (bn:bigint->inexact b) b)
              ) ;native:/
            ) ;if
          ) ;let*
        ) ;if
      ) ;let-values
    ) ;let
   ) ;
   (else (native:/ (if (bigint? a) (bn:bigint->inexact a) a)
           (if (bigint? b) (bn:bigint->inexact b) b)
         ) ;native:/
   ) ;else
  ) ;cond
) ;define

(define (bn:+ . args)
  (if (null? args)
    0
    (let loop
      ((acc (car args)) (rest (cdr args)))
      (if (null? rest) acc (loop (bn:add-2 acc (car rest)) (cdr rest)))
    ) ;let
  ) ;if
) ;define

(define (bn:- first . rest)
  (if (null? rest)
    (bn:negate-1 first)
    (let loop
      ((acc first) (xs rest))
      (if (null? xs) acc (loop (bn:sub-2 acc (car xs)) (cdr xs)))
    ) ;let
  ) ;if
) ;define

(define (bn:* . args)
  (if (null? args)
    1
    (let loop
      ((acc (car args)) (rest (cdr args)))
      (if (null? rest) acc (loop (bn:mul-2 acc (car rest)) (cdr rest)))
    ) ;let
  ) ;if
) ;define

(define (bn:/ first . rest)
  (if (null? rest)
    (bn:div-2 1 first)
    (let loop
      ((acc first) (xs rest))
      (if (null? xs) acc (loop (bn:div-2 acc (car xs)) (cdr xs)))
    ) ;let
  ) ;if
) ;define

(define (bn:abs x)
  (cond ((bigint? x) (bn:demote-bigint (bigint-abs x)))
        ((and (native:integer? x) (native:exact? x) (native:= x INT64-MIN))
         (bigint-negate INT64-MIN-BIG)
        ) ;
        (else (native:abs x))
  ) ;cond
) ;define

(define (bn:quotient a b)
  (cond
   ((and (bn:exact-int? a) (bn:exact-int? b))
    (if (or (bigint? a) (bigint? b) (and (native:= a INT64-MIN) (native:= b -1)))
      (bn:demote-bigint (bigint-quotient (bn:to-bigint a) (bn:to-bigint b)))
      (native:quotient a b)
    ) ;if
   ) ;
   ((and (bn:any-int? a) (bn:any-int? b))
    (bn:int-res->inexact (bigint-quotient (bn:any-int->bigint a) (bn:any-int->bigint b))
    ) ;bn:int-res->inexact
   ) ;
   (else (native:quotient (if (bigint? a) (bn:bigint->inexact a) a)
           (if (bigint? b) (bn:bigint->inexact b) b)
         ) ;native:quotient
   ) ;else
  ) ;cond
) ;define

(define (bn:remainder a b)
  (cond
   ((and (bn:exact-int? a) (bn:exact-int? b))
    (if (or (bigint? a) (bigint? b))
      (bn:demote-bigint (bigint-remainder (bn:to-bigint a) (bn:to-bigint b)))
      (native:remainder a b)
    ) ;if
   ) ;
   ((and (bn:any-int? a) (bn:any-int? b))
    (bn:int-res->inexact (bigint-remainder (bn:any-int->bigint a) (bn:any-int->bigint b))
    ) ;bn:int-res->inexact
   ) ;
   (else (native:remainder (if (bigint? a) (bn:bigint->inexact a) a)
           (if (bigint? b) (bn:bigint->inexact b) b)
         ) ;native:remainder
   ) ;else
  ) ;cond
) ;define

(define (bn:modulo a b)
  (cond
   ((and (bn:exact-int? a) (bn:exact-int? b))
    (if (or (bigint? a) (bigint? b))
      (let* ((ba (bn:to-bigint a)) (bb (bn:to-bigint b)) (r (bigint-remainder ba bb)))
        (bn:demote-bigint
          (if (or (bigint-zero? r) (eqv? (bigint-negative? r) (bigint-negative? bb)))
            r
            (bigint-add r bb)
          ) ;if
        ) ;bn:demote-bigint
      ) ;let*
      (native:modulo a b)
    ) ;if
   ) ;
   ((and (bn:any-int? a) (bn:any-int? b))
    (let* ((ba (bn:any-int->bigint a))
           (bb (bn:any-int->bigint b))
           (r (bigint-remainder ba bb))
          ) ;
      (bn:int-res->inexact
        (if (or (bigint-zero? r) (eqv? (bigint-negative? r) (bigint-negative? bb)))
          r
          (bigint-add r bb)
        ) ;if
      ) ;bn:int-res->inexact
    ) ;let*
   ) ;
   (else (native:modulo (if (bigint? a) (bn:bigint->inexact a) a)
           (if (bigint? b) (bn:bigint->inexact b) b)
         ) ;native:modulo
   ) ;else
  ) ;cond
) ;define

(define (bn:gcd . args)
  (if (null? args)
    0
    (let loop
      ((acc (bn:abs (car args))) (xs (cdr args)))
      (if (null? xs)
        acc
        (let ((y (bn:abs (car xs))))
          (cond
           ((and (bn:exact-int? acc) (bn:exact-int? y))
            (loop (bn:demote-bigint (bigint-gcd (bn:to-bigint acc) (bn:to-bigint y)))
              (cdr xs)
            ) ;loop
           ) ;
           ((and (bn:any-int? acc) (bn:any-int? y))
            (loop (bn:int-res->inexact (bigint-gcd (bn:any-int->bigint acc) (bn:any-int->bigint y))
                  ) ;bn:int-res->inexact
              (cdr xs)
            ) ;loop
           ) ;
           (else
             (loop (native:gcd (if (bigint? acc) (bn:bigint->inexact acc) acc)
                     (if (bigint? y) (bn:bigint->inexact y) y)
                   ) ;native:gcd
               (cdr xs)
             ) ;loop
           ) ;else
          ) ;cond
        ) ;let
      ) ;if
    ) ;let
  ) ;if
) ;define

(define (bn:lcm . args)
  (if (null? args)
    1
    (let loop
      ((acc (bn:abs (car args))) (xs (cdr args)))
      (if (null? xs)
        acc
        (let ((y (bn:abs (car xs))))
          (cond
           ((or (and (bigint? acc) (bigint-zero? acc))
              (and (not (bigint? acc)) (native:zero? acc))
              (and (bigint? y) (bigint-zero? y))
              (and (not (bigint? y)) (native:zero? y))
            ) ;or
            (loop (if (and (bn:exact-int? acc) (bn:exact-int? y)) 0 0.0) (cdr xs))
           ) ;
           ((and (bn:exact-int? acc) (bn:exact-int? y))
            (let* ((ba (bn:to-bigint acc)) (bb (bn:to-bigint y)) (g (bigint-gcd ba bb)))
              (loop (bn:demote-bigint (bigint-multiply (bigint-quotient ba g) bb)) (cdr xs))
            ) ;let*
           ) ;
           ((and (bn:any-int? acc) (bn:any-int? y))
            (let* ((ba (bn:any-int->bigint acc))
                   (bb (bn:any-int->bigint y))
                   (g (bigint-gcd ba bb))
                  ) ;
              (loop (bn:int-res->inexact (bigint-multiply (bigint-quotient ba g) bb))
                (cdr xs)
              ) ;loop
            ) ;let*
           ) ;
           (else
             (loop (native:lcm (if (bigint? acc) (bn:bigint->inexact acc) acc)
                     (if (bigint? y) (bn:bigint->inexact y) y)
                   ) ;native:lcm
               (cdr xs)
             ) ;loop
           ) ;else
          ) ;cond
        ) ;let
      ) ;if
    ) ;let
  ) ;if
) ;define

(define (bn:expt base exp)
  (cond
   ((and (bn:exact-int? base)
      (bn:exact-int? exp)
      (not (if (bigint? exp) (bigint-negative? exp) (native:negative? exp)))
    ) ;and
    (cond
     ((and (native:integer? exp) (native:exact? exp))
      (bn:demote-bigint (bigint-expt (bn:to-bigint base) exp))
     ) ;
     ((if (bigint? base) (bigint-zero? base) (native:zero? base)) 0)
     ((and (not (bigint? base)) (native:= base 1)) 1)
     ((and (not (bigint? base)) (native:= base -1)) (if (bigint-even? exp) 1 -1))
     (else (native:expt (if (bigint? base) (bn:bigint->inexact base) (native:inexact base))
             (if (bigint? exp) (bn:bigint->inexact exp) (native:inexact exp))
           ) ;native:expt
     ) ;else
    ) ;cond
   ) ;
   ((and (bn:exact-int? base) (bn:exact-int? exp))
    (cond
     ((and (not (bigint? base)) (native:= base 1)) 1)
     ((and (not (bigint? base)) (native:= base -1))
      (if (bigint-even? (bn:to-bigint exp)) 1 -1)
     ) ;
     (else (bn:div-2 1 (bn:expt base (bn:negate-1 exp))))
    ) ;cond
   ) ;
   (else (native:expt (if (bigint? base) (bn:bigint->inexact base) base)
           (if (bigint? exp) (bn:bigint->inexact exp) exp)
         ) ;native:expt
   ) ;else
  ) ;cond
) ;define
