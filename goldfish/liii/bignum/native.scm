;;; liii/bignum/native.scm - 保存 (scheme base) 原生数值过程引用
;;;
;;; Copyright (c) 2026 Liii Network
;;; All Rights Reserved

(define-library (liii bignum native)
  (import (scheme base))

  (export base:number? base:complex? base:real? base:rational? base:integer?
    base:exact? base:inexact? base:zero? base:positive? base:negative?
    base:odd? base:even? base:exact base:inexact base:number->string
    base:string->number base:eqv? base:= base:< base:> base:<= base:>=
    base:+ base:- base:* base:/ base:abs base:quotient base:remainder
    base:modulo base:gcd base:lcm base:expt base:floor base:ceiling
    base:truncate base:round base:numerator base:denominator
  ) ;export

  (begin
    (define base:number? number?)
    (define base:complex? complex?)
    (define base:real? real?)
    (define base:rational? rational?)
    (define base:integer? integer?)
    (define base:exact? exact?)
    (define base:inexact? inexact?)
    (define base:zero? zero?)
    (define base:positive? positive?)
    (define base:negative? negative?)
    (define base:odd? odd?)
    (define base:even? even?)
    (define base:exact exact)
    (define base:inexact inexact)
    (define base:number->string number->string)
    (define base:string->number string->number)
    (define base:eqv? eqv?)
    (define base:= =)
    (define base:< <)
    (define base:> >)
    (define base:<= <=)
    (define base:>= >=)
    (define base:+ +)
    (define base:- -)
    (define base:* *)
    (define base:/ /)
    (define base:abs abs)
    (define base:quotient quotient)
    (define base:remainder remainder)
    (define base:modulo modulo)
    (define base:gcd gcd)
    (define base:lcm lcm)
    (define base:expt expt)
    (define base:floor floor)
    (define base:ceiling ceiling)
    (define base:truncate truncate)
    (define base:round round)
    (define base:numerator numerator)
    (define base:denominator denominator)
  ) ;begin
) ;define-library
