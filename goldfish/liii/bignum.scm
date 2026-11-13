;;; liii/bignum.scm - 运行时 bignum 数值塔适配模块
;;;
;;; 配合 `--mode=liii+bignum` / `--mode=r7rs+bignum` 使用，
;;; 基于 (liii bigint) 与 (liii decimal) 提供任意精度整数数值塔。
;;;
;;; Copyright (c) 2026 Liii Network
;;; All Rights Reserved

(define-library (liii bignum)
  (import (scheme base) (liii bigint) (liii decimal))

  (export bignum? number? complex? real? rational? integer? exact? inexact?
    exact-integer? zero? positive? negative? odd? even? exact inexact
    exact->inexact inexact->exact number->string string->number eqv? = < > <= >=
    max min + - * / abs quotient remainder modulo gcd lcm expt floor ceiling
    truncate round numerator denominator
  ) ;export

  (include "bignum/native.scm" "bignum/arithmetic.scm")

  (begin
    (define (bignum? x)
      (bigint? x)
    ) ;define

    ;;; ---------- 谓词 ----------
    (define (bn:number? x)
      (or (bigint? x) (native:number? x))
    ) ;define

    (define (bn:complex? x)
      (or (bigint? x) (native:complex? x))
    ) ;define

    (define (bn:real? x)
      (or (bigint? x) (native:real? x))
    ) ;define

    (define (bn:rational? x)
      (or (bigint? x) (native:rational? x))
    ) ;define

    (define (bn:integer? x)
      (or (bigint? x) (native:integer? x))
    ) ;define

    (define (bn:exact? x)
      (or (bigint? x) (native:exact? x))
    ) ;define

    (define (bn:inexact? x)
      (and (not (bigint? x)) (native:inexact? x))
    ) ;define

    (define (bn:exact-integer? x)
      (or (bigint? x) (and (native:integer? x) (native:exact? x)))
    ) ;define

    (define (bn:zero? x)
      (if (bigint? x) (bigint-zero? x) (native:zero? x))
    ) ;define

    (define (bn:positive? x)
      (if (bigint? x) (bigint-positive? x) (native:positive? x))
    ) ;define

    (define (bn:negative? x)
      (if (bigint? x) (bigint-negative? x) (native:negative? x))
    ) ;define

    (define (bn:even? x)
      (if (bigint? x) (bigint-even? x) (native:even? x))
    ) ;define

    (define (bn:odd? x)
      (if (bigint? x) (bigint-odd? x) (native:odd? x))
    ) ;define

    ;;; ---------- 转换与等价 ----------
    (define (bn:exact x)
      (if (bigint? x) x (native:exact x))
    ) ;define

    (define (bn:inexact x)
      (if (bigint? x) (bn:bigint->inexact x) (native:inexact x))
    ) ;define

    (define (bn:number->string x . rest)
      (if (null? rest)
        (if (bigint? x) (bigint->string x) (native:number->string x))
        (if (bigint? x)
          (bigint->string x (car rest))
          (apply native:number->string x rest)
        ) ;if
      ) ;if
    ) ;define

    (define (bn:string->number str . rest)
      (apply native:string->number str rest)
    ) ;define

    (define (bn:eqv? a b)
      (cond ((and (bigint? a) (bigint? b)) (bigint=? a b))
            ((or (bigint? a) (bigint? b)) #f)
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
       ((bigint? a)
        (if (and (native:rational? b) (native:exact? b))
          (bigint=? (bigint-multiply a (integer->bigint (native:denominator b)))
            (integer->bigint (native:numerator b))
          ) ;bigint=?
          (native:= (bn:bigint->inexact a) b)
        ) ;if
       ) ;
       ((bigint? b)
        (if (and (native:rational? a) (native:exact? a))
          (bigint=? (integer->bigint (native:numerator a))
            (bigint-multiply b (integer->bigint (native:denominator a)))
          ) ;bigint=?
          (native:= a (bn:bigint->inexact b))
        ) ;if
       ) ;
       (else (native:= a b))
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
       ((bigint? a)
        (if (and (native:rational? b) (native:exact? b))
          (bigint<? (bigint-multiply a (integer->bigint (native:denominator b)))
            (integer->bigint (native:numerator b))
          ) ;bigint<?
          (native:< (bn:bigint->inexact a) b)
        ) ;if
       ) ;
       ((bigint? b)
        (if (and (native:rational? a) (native:exact? a))
          (bigint<? (integer->bigint (native:numerator a))
            (bigint-multiply b (integer->bigint (native:denominator a)))
          ) ;bigint<?
          (native:< a (bn:bigint->inexact b))
        ) ;if
       ) ;
       (else (native:< a b))
      ) ;cond
    ) ;define

    (define (bn:make-cmp-op cmp2)
      (lambda (a b . rest)
        (let loop
          ((x a) (y b) (r rest))
          (if (cmp2 x y) (if (null? r) #t (loop y (car r) (cdr r))) #f)
        ) ;let
      ) ;lambda
    ) ;define

    (define bn:= (bn:make-cmp-op bn:num-eq-2))
    (define bn:< (bn:make-cmp-op bn:num-lt-2))
    (define bn:> (bn:make-cmp-op (lambda (a b) (bn:num-lt-2 b a))))
    (define bn:<= (bn:make-cmp-op (lambda (a b) (not (bn:num-lt-2 b a)))))
    (define bn:>= (bn:make-cmp-op (lambda (a b) (not (bn:num-lt-2 a b)))))

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

    ;;; ---------- 注册 C 层读写钩子 ----------
    (when (defined? 's7-set-bignum-hooks!)
      (s7-set-bignum-hooks! '<bigint>
        (lambda (str radix) (guard (err (#t #f)) (string->bigint str radix)))
        bigint->string
      ) ;s7-set-bignum-hooks!
    ) ;when
  ) ;begin
) ;define-library
