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
   ((and (or (bigint? a) (bigrat? a) (bigint? b) (bigrat? b))
      (bn:exact-rat-any? a)
      (bn:exact-rat-any? b)
    ) ;and
    (let ((n1 (bn:exact-rat-num a))
          (d1 (bn:exact-rat-den a))
          (n2 (bn:exact-rat-num b))
          (d2 (bn:exact-rat-den b))
         ) ;
      (bn:make-rat (bigint-add (bigint-multiply n1 d2) (bigint-multiply n2 d1))
        (bigint-multiply d1 d2)
      ) ;bn:make-rat
    ) ;let
   ) ;
   (else (native:+ (bn:to-inexact a) (bn:to-inexact b)))
  ) ;cond
) ;define

(define (bn:negate-1 a)
  (cond ((bigint? a) (bn:demote-bigint (bigint-negate a)))
        ((bigrat? a) (bn:make-rat (bigint-negate (bigrat-num a)) (bigrat-den a)))
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
   ((and (or (bigint? a) (bigrat? a) (bigint? b) (bigrat? b))
      (bn:exact-rat-any? a)
      (bn:exact-rat-any? b)
    ) ;and
    (let ((n1 (bn:exact-rat-num a))
          (d1 (bn:exact-rat-den a))
          (n2 (bn:exact-rat-num b))
          (d2 (bn:exact-rat-den b))
         ) ;
      (bn:make-rat (bigint-subtract (bigint-multiply n1 d2) (bigint-multiply n2 d1))
        (bigint-multiply d1 d2)
      ) ;bn:make-rat
    ) ;let
   ) ;
   (else (native:- (bn:to-inexact a) (bn:to-inexact b)))
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
   ((and (or (bigint? a) (bigrat? a) (bigint? b) (bigrat? b))
      (bn:exact-rat-any? a)
      (bn:exact-rat-any? b)
    ) ;and
    (bn:make-rat (bigint-multiply (bn:exact-rat-num a) (bn:exact-rat-num b))
      (bigint-multiply (bn:exact-rat-den a) (bn:exact-rat-den b))
    ) ;bn:make-rat
   ) ;
   (else (native:* (bn:to-inexact a) (bn:to-inexact b)))
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
    (bn:make-rat (bn:to-bigint a) (bn:to-bigint b))
   ) ;
   ((and (or (bigint? a) (bigrat? a) (bigint? b) (bigrat? b))
      (bn:exact-rat-any? a)
      (bn:exact-rat-any? b)
    ) ;and
    (bn:make-rat (bigint-multiply (bn:exact-rat-num a) (bn:exact-rat-den b))
      (bigint-multiply (bn:exact-rat-den a) (bn:exact-rat-num b))
    ) ;bn:make-rat
   ) ;
   (else (native:/ (bn:to-inexact a) (bn:to-inexact b)))
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
        ((bigrat? x) (if (bigint-negative? (bigrat-num x)) (bn:negate-1 x) x))
        ((and (native:integer? x) (native:exact? x) (native:= x INT64-MIN))
         (bigint-negate INT64-MIN-BIG)
        ) ;
        (else (native:abs x))
  ) ;cond
) ;define
