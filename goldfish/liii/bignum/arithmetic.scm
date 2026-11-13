;;; liii/bignum/arithmetic.scm - bignum 算术运算与溢出升降级（由 liii/bignum.scm include）
;;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;  http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

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

(define (bn:exact-rat? x)
  (and (not (bigint? x)) (native:rational? x) (native:exact? x))
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
   ((and (bigint? a) (bn:exact-rat? b))
    (let ((den (native:denominator b)))
      (bn:div-2 (bn:add-2 (bn:mul-2 a den) (native:numerator b)) den)
    ) ;let
   ) ;
   ((and (bn:exact-rat? a) (bigint? b))
    (let ((den (native:denominator a)))
      (bn:div-2 (bn:add-2 (native:numerator a) (bn:mul-2 b den)) den)
    ) ;let
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
   ((and (bigint? a) (bn:exact-rat? b))
    (let ((den (native:denominator b)))
      (bn:div-2 (bn:sub-2 (bn:mul-2 a den) (native:numerator b)) den)
    ) ;let
   ) ;
   ((and (bn:exact-rat? a) (bigint? b))
    (let ((den (native:denominator a)))
      (bn:div-2 (bn:sub-2 (native:numerator a) (bn:mul-2 b den)) den)
    ) ;let
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
   ((and (bigint? a) (bn:exact-rat? b))
    (bn:div-2 (bn:mul-2 a (native:numerator b)) (native:denominator b))
   ) ;
   ((and (bn:exact-rat? a) (bigint? b))
    (bn:div-2 (bn:mul-2 (native:numerator a) b) (native:denominator a))
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
   ((and (bigint? a) (bn:exact-rat? b))
    (bn:div-2 (bn:mul-2 a (native:denominator b)) (native:numerator b))
   ) ;
   ((and (bn:exact-rat? a) (bigint? b))
    (bn:div-2 (native:numerator a) (bn:mul-2 (native:denominator a) b))
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
