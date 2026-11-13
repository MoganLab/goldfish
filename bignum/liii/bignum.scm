;;; liii/bignum.scm - 运行时 bignum 数值塔适配模块
;;;
;;; 必须开启 `+bignum` 模式使用（例如 `--mode=r7rs+bignum` / `--mode=liii+bignum`），
;;; 基于 (liii bigint)、(liii bigrat)、(liii bigcomplex) 与 (liii decimal)
;;; 提供完整的 R7RS 任意精度整数、有理数与复数数值塔，并配合 C 层读写钩子使用。
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

(define-library (liii bignum)
  (import (scheme base)
    (scheme complex)
    (scheme inexact)
    (liii bigint)
    (liii bigrat)
    (liii bigcomplex)
    (liii decimal)
  ) ;import

  (export bignum? number? complex? real? rational? integer? exact? inexact?
    exact-integer? zero? positive? negative? odd? even? exact inexact
    exact->inexact inexact->exact number->string string->number eqv? = < > <= >=
    max min + - * / abs quotient remainder modulo gcd lcm expt floor ceiling
    truncate round numerator denominator make-rectangular real-part imag-part
    square exact-integer-sqrt floor/ floor-quotient floor-remainder truncate/
    truncate-quotient truncate-remainder sqrt magnitude angle finite? infinite?
    nan?
  ) ;export

  (include "bignum/native.scm" "bignum/convert.scm" "bignum/rat.scm"
    "bignum/complex.scm" "bignum/arithmetic.scm" "bignum/integer.scm"
    "bignum/parse.scm" "bignum/extra.scm"
  ) ;include

  (begin
    (define (bignum? x)
      (or (bigint? x) (bigrat? x) (bigcomplex? x))
    ) ;define

    ;;; ---------- 谓词 ----------
    (define (bn:number? x)
      (or (bigint? x) (bigrat? x) (bigcomplex? x) (native:number? x))
    ) ;define

    (define (bn:complex? x)
      (or (bigint? x) (bigrat? x) (bigcomplex? x) (native:complex? x))
    ) ;define

    (define (bn:real? x)
      (or (bigint? x) (bigrat? x) (and (not (bigcomplex? x)) (native:real? x)))
    ) ;define

    (define (bn:rational? x)
      (or (bigint? x) (bigrat? x) (and (not (bigcomplex? x)) (native:rational? x)))
    ) ;define

    (define (bn:integer? x)
      (or (bigint? x)
        (and (not (bigrat? x)) (not (bigcomplex? x)) (native:integer? x))
      ) ;or
    ) ;define

    (define (bn:exact? x)
      (or (bigint? x) (bigrat? x) (bigcomplex? x) (native:exact? x))
    ) ;define

    (define (bn:inexact? x)
      (and (not (bigint? x))
        (not (bigrat? x))
        (not (bigcomplex? x))
        (native:inexact? x)
      ) ;and
    ) ;define

    (define (bn:exact-integer? x)
      (or (bigint? x)
        (and (not (bigrat? x))
          (not (bigcomplex? x))
          (native:integer? x)
          (native:exact? x)
        ) ;and
      ) ;or
    ) ;define

    (define (bn:zero? x)
      (cond ((bigint? x) (bigint-zero? x))
            ((or (bigrat? x) (bigcomplex? x)) #f)
            (else (native:zero? x))
      ) ;cond
    ) ;define

    (define (bn:positive? x)
      (cond ((bigint? x) (bigint-positive? x))
            ((bigrat? x) (bigrat-positive? x))
            ((bigcomplex? x) (native:positive? 0.0+1.0i))
            (else (native:positive? x))
      ) ;cond
    ) ;define

    (define (bn:negative? x)
      (cond ((bigint? x) (bigint-negative? x))
            ((bigrat? x) (bigrat-negative? x))
            ((bigcomplex? x) (native:negative? 0.0+1.0i))
            (else (native:negative? x))
      ) ;cond
    ) ;define

    (define (bn:even? x)
      (cond ((bigint? x) (bigint-even? x))
            ((or (bigrat? x) (bigcomplex? x)) (native:even? 1/2))
            (else (native:even? x))
      ) ;cond
    ) ;define

    (define (bn:odd? x)
      (cond ((bigint? x) (bigint-odd? x))
            ((or (bigrat? x) (bigcomplex? x)) (native:odd? 1/2))
            (else (native:odd? x))
      ) ;cond
    ) ;define

    ;;; ---------- 转换与等价 ----------
    (define (bn:exact x)
      (cond ((or (bigint? x) (bigrat? x) (bigcomplex? x)) x)
            ((bn:complex-any? x)
             (bn:make-complex (bn:exact (native:real-part x))
               (bn:exact (native:imag-part x))
             ) ;bn:make-complex
            ) ;
            ((and (bn:finite-real? x)
               (native:inexact? x)
               (not (native:< (native:abs x) 9007199254740992.0))
             ) ;and
             (bn:inexact-large->bigint x)
            ) ;
            (else (native:exact x))
      ) ;cond
    ) ;define

    (define (bn:inexact x)
      (cond ((bigint? x) (bn:bigint->inexact x))
            ((bigrat? x) (bn:bigrat->inexact x))
            ((bigcomplex? x) (bn:bigcomplex->inexact x))
            (else (native:inexact x))
      ) ;cond
    ) ;define

    (define (bn:number->string x . rest)
      (if (null? rest)
        (cond ((bigint? x) (bigint->string x))
              ((bigrat? x) (bigrat->string x))
              ((bigcomplex? x) (bigcomplex->string x))
              (else (native:number->string x))
        ) ;cond
        (cond ((bigint? x) (bigint->string x (car rest)))
              ((bigrat? x) (bigrat->string x (car rest)))
              ((bigcomplex? x) (bigcomplex->string x (car rest)))
              (else (apply native:number->string x rest))
        ) ;cond
      ) ;if
    ) ;define

    (define (bn:eqv? a b)
      (cond ((and (bigint? a) (bigint? b)) (bigint=? a b))
            ((and (bigrat? a) (bigrat? b)) (bigrat=? a b))
            ((and (bigcomplex? a) (bigcomplex? b))
             (and (bn:eqv? (bigcomplex-real a) (bigcomplex-real b))
               (bn:eqv? (bigcomplex-imag a) (bigcomplex-imag b))
             ) ;and
            ) ;
            ((or (bigint? a)
               (bigint? b)
               (bigrat? a)
               (bigrat? b)
               (bigcomplex? a)
               (bigcomplex? b)
             ) ;or
             #f
            ) ;
            (else (native:eqv? a b))
      ) ;cond
    ) ;define

    ;;; ---------- 比较 ----------
    (define (bn:num-eq-2 a b)
      (cond
       ((and (bn:exact-int? a) (bn:exact-int? b))
        (if (or (bigint? a) (bigint? b))
          (bigint=? (bn:to-bigint a) (bn:to-bigint b))
          (native:= a b)
        ) ;if
       ) ;
       ((and (bn:complex-any? a) (bn:number? b)) (bn:complex-eq? a b))
       ((and (bn:complex-any? b) (bn:number? a)) (bn:complex-eq? a b))
       ((and (bn:exact-rat-any? a) (bn:exact-rat-any? b))
        (bigint=? (bigint-multiply (bn:exact-rat-num a) (bn:exact-rat-den b))
          (bigint-multiply (bn:exact-rat-num b) (bn:exact-rat-den a))
        ) ;bigint=?
       ) ;
       ((and (or (bigint? a) (bigrat? a)) (native:real? b))
        (if (bn:finite-real? b)
          (if (not (native:< (native:abs b) 9007199254740992.0))
            (bn:num-eq-2 a (bn:inexact-large->bigint b))
            (and (bigrat? a) (bn:num-eq-2 a (native:exact b)))
          ) ;if
          #f
        ) ;if
       ) ;
       ((and (or (bigint? b) (bigrat? b)) (native:real? a)) (bn:num-eq-2 b a))
       (else (native:= (bn:to-inexact a) (bn:to-inexact b)))
      ) ;cond
    ) ;define

    (define (bn:num-lt-2 a b)
      (cond
       ((and (bn:exact-int? a) (bn:exact-int? b))
        (if (or (bigint? a) (bigint? b))
          (bigint<? (bn:to-bigint a) (bn:to-bigint b))
          (native:< a b)
        ) ;if
       ) ;
       ((and (bn:exact-rat-any? a) (bn:exact-rat-any? b))
        (bigint<? (bigint-multiply (bn:exact-rat-num a) (bn:exact-rat-den b))
          (bigint-multiply (bn:exact-rat-num b) (bn:exact-rat-den a))
        ) ;bigint<?
       ) ;
       ((and (or (bigint? a) (bigrat? a)) (native:real? b))
        (if (bn:finite-real? b)
          (if (not (native:< (native:abs b) 9007199254740992.0))
            (bn:num-lt-2 a (bn:inexact-large->bigint b))
            (if (bigint? a) (bigint-negative? a) (bn:num-lt-2 a (native:exact b)))
          ) ;if
          (native:= b +inf.0)
        ) ;if
       ) ;
       ((and (or (bigint? b) (bigrat? b)) (native:real? a))
        (if (bn:finite-real? a)
          (if (not (native:< (native:abs a) 9007199254740992.0))
            (bn:num-lt-2 (bn:inexact-large->bigint a) b)
            (if (bigint? b) (bigint-positive? b) (bn:num-lt-2 (native:exact a) b))
          ) ;if
          (native:= a -inf.0)
        ) ;if
       ) ;
       (else (native:< (bn:to-inexact a) (bn:to-inexact b)))
      ) ;cond
    ) ;define

    (define (bn:make-cmp-op cmp2 real-only?)
      (lambda (a . rest)
        (if (null? rest)
          (begin
            (if real-only?
              (unless (bn:real? a)
                (native:< a 0)
              ) ;unless
              (unless (bn:number? a)
                (native:= a 0)
              ) ;unless
            ) ;if
            #t
          ) ;begin
          (let loop
            ((x a) (y (car rest)) (r (cdr rest)))
            (if (cmp2 x y) (if (null? r) #t (loop y (car r) (cdr r))) #f)
          ) ;let
        ) ;if
      ) ;lambda
    ) ;define

    (define bn:= (bn:make-cmp-op bn:num-eq-2 #f))
    (define bn:< (bn:make-cmp-op bn:num-lt-2 #t))
    (define bn:> (bn:make-cmp-op (lambda (a b) (bn:num-lt-2 b a)) #t))
    (define bn:<=
      (bn:make-cmp-op (lambda (a b) (or (bn:num-lt-2 a b) (bn:num-eq-2 a b))) #t)
    ) ;define
    (define bn:>=
      (bn:make-cmp-op (lambda (a b) (or (bn:num-lt-2 b a) (bn:num-eq-2 a b))) #t)
    ) ;define

    (define (bn:max first . rest)
      (let loop
        ((best first) (xs rest) (has-inexact (bn:inexact? first)))
        (if (null? xs)
          (if (and has-inexact (bn:exact? best)) (bn:inexact best) best)
          (let* ((x (car xs)) (inx (or has-inexact (bn:inexact? x))))
            (loop (if (bn:num-lt-2 best x) x best) (cdr xs) inx)
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (bn:min first . rest)
      (let loop
        ((best first) (xs rest) (has-inexact (bn:inexact? first)))
        (if (null? xs)
          (if (and has-inexact (bn:exact? best)) (bn:inexact best) best)
          (let* ((x (car xs)) (inx (or has-inexact (bn:inexact? x))))
            (loop (if (bn:num-lt-2 x best) x best) (cdr xs) inx)
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    ;;; ---------- 导出绑定 ----------
    (define number? bn:number?)
    (define complex? bn:complex?)
    (define real? bn:real?)
    (define rational? bn:rational?)
    (define integer? bn:integer?)
    (define exact? bn:exact?)
    (define inexact? bn:inexact?)
    (define exact-integer? bn:exact-integer?)
    (define zero? bn:zero?)
    (define positive? bn:positive?)
    (define negative? bn:negative?)
    (define odd? bn:odd?)
    (define even? bn:even?)
    (define exact bn:exact)
    (define inexact bn:inexact)
    (define exact->inexact bn:inexact)
    (define inexact->exact bn:exact)
    (define number->string bn:number->string)
    (define string->number bn:string->number)
    (define eqv? bn:eqv?)
    (define = bn:=)
    (define < bn:<)
    (define > bn:>)
    (define <= bn:<=)
    (define >= bn:>=)
    (define max bn:max)
    (define min bn:min)
    (define + bn:+)
    (define - bn:-)
    (define * bn:*)
    (define / bn:/)
    (define abs bn:abs)
    (define quotient bn:quotient)
    (define remainder bn:remainder)
    (define modulo bn:modulo)
    (define gcd bn:gcd)
    (define lcm bn:lcm)
    (define expt bn:expt)
    (define floor bn:floor)
    (define ceiling bn:ceiling)
    (define truncate bn:truncate)
    (define round bn:round)
    (define numerator bn:numerator)
    (define denominator bn:denominator)
    (define make-rectangular bn:make-complex)
    (define real-part bn:c-real)
    (define imag-part bn:c-imag)
    (define square bn:square)
    (define exact-integer-sqrt bn:exact-integer-sqrt)
    (define floor/ bn:floor/)
    (define floor-quotient bn:floor-quotient)
    (define floor-remainder bn:floor-remainder)
    (define truncate/ bn:truncate/)
    (define truncate-quotient bn:truncate-quotient)
    (define truncate-remainder bn:truncate-remainder)
    (define sqrt bn:sqrt)
    (define magnitude bn:magnitude)
    (define angle bn:angle)
    (define finite? bn:finite?)
    (define infinite? bn:infinite?)
    (define nan? bn:nan?)

    (define (bn:write-bignum x)
      (cond ((bigrat? x) (bigrat->string x))
            ((bigcomplex? x) (bigcomplex->string x))
            (else (bigint->string x))
      ) ;cond
    ) ;define

    ;;; ---------- 注册 C 层读写钩子 ----------
    (when (defined? 's7-set-bignum-hooks!)
      (s7-set-bignum-hooks! '(<bigint> <bigrat> <bigcomplex>)
        bn:read-literal
        bn:write-bignum
      ) ;s7-set-bignum-hooks!
    ) ;when
  ) ;begin
) ;define-library
