;;; liii/bignum/native.scm - 原生数值塔过程别名（由 liii/bignum.scm include）
;;;
;;; 在 bignum 模式覆写 (scheme base) 数值过程之前保存原生绑定，
;;; 供后续算术回退与非 bignum 值运算使用。
;;;
;;; Copyright (c) 2026 Liii Network
;;; All Rights Reserved

(define native:number? number?)

(define native:complex? complex?)

(define native:real? real?)

(define native:rational? rational?)

(define native:integer? integer?)

(define native:exact? exact?)

(define native:inexact? inexact?)

(define native:zero? zero?)

(define native:positive? positive?)

(define native:negative? negative?)

(define native:odd? odd?)

(define native:even? even?)

(define native:exact exact)

(define native:inexact inexact)

(define native:number->string number->string)

(define native:string->number string->number)

(define native:eqv? eqv?)

(define native:= =)

(define native:< <)

(define native:> >)

(define native:+ +)

(define native:- -)

(define native:* *)

(define native:/ /)

(define native:abs abs)

(define native:quotient quotient)

(define native:remainder remainder)

(define native:modulo modulo)

(define native:gcd gcd)

(define native:lcm lcm)

(define native:expt expt)

(define native:floor floor)

(define native:ceiling ceiling)

(define native:truncate truncate)

(define native:round round)

(define native:numerator numerator)

(define native:denominator denominator)
