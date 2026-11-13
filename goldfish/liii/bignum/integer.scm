;;; liii/bignum/integer.scm - bignum 整数除法、数论与幂运算（由 liii/bignum.scm include）
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
      (bn:demote-bigint (bigint-modulo (bn:to-bigint a) (bn:to-bigint b)))
      (native:modulo a b)
    ) ;if
   ) ;
   ((and (bn:any-int? a) (bn:any-int? b))
    (bn:int-res->inexact (bigint-modulo (bn:any-int->bigint a) (bn:any-int->bigint b))
    ) ;bn:int-res->inexact
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
   ((and (bigrat? base) (native:integer? exp) (native:exact? exp))
    (cond ((native:zero? exp) 1)
          ((native:positive? exp)
           (bn:make-rat (bigint-expt (bigrat-num base) exp)
             (bigint-expt (bigrat-den base) exp)
           ) ;bn:make-rat
          ) ;
          (else (bn:make-rat (bigint-expt (bigrat-den base) (native:- exp))
                  (bigint-expt (bigrat-num base) (native:- exp))
                ) ;bn:make-rat
          ) ;else
    ) ;cond
   ) ;
   (else (native:expt (bn:to-inexact base) (bn:to-inexact exp)))
  ) ;cond
) ;define
