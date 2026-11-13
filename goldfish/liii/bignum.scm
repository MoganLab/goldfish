;;; liii/bignum.scm - 运行时 bignum 数值塔适配模块
;;;
;;; 配合 `--mode=liii+bignum` / `--mode=r7rs+bignum` 使用，
;;; 基于 (liii bigint) 与 (liii decimal) 提供任意精度整数数值塔。
;;;
;;; Copyright (c) 2026 Liii Network
;;; All Rights Reserved

(define-library (liii bignum)
  (import (scheme base)
    (liii bignum native)
    (liii bignum arithmetic)
    (liii bigint)
    (liii decimal)
  ) ;import

  (export bignum? number? complex? real? rational? integer? exact? inexact?
    exact-integer? zero? positive? negative? odd? even? exact inexact
    exact->inexact inexact->exact number->string string->number eqv? = < > <= >=
    max min + - * / abs quotient remainder modulo gcd lcm expt floor ceiling
    truncate round numerator denominator
  ) ;export

  (begin
    (define (bignum? x)
      (bigint? x)
    ) ;define

    ;;; ---------- 谓词 ----------
    (define (number? x)
      (or (bigint? x) (base:number? x))
    ) ;define

    (define (complex? x)
      (or (bigint? x) (base:complex? x))
    ) ;define

    (define (real? x)
      (or (bigint? x) (base:real? x))
    ) ;define

    (define (rational? x)
      (or (bigint? x) (base:rational? x))
    ) ;define

    (define (integer? x)
      (or (bigint? x) (base:integer? x))
    ) ;define

    (define (exact? x)
      (or (bigint? x) (base:exact? x))
    ) ;define

    (define (inexact? x)
      (and (not (bigint? x)) (base:inexact? x))
    ) ;define

    (define (exact-integer? x)
      (or (bigint? x) (and (base:integer? x) (base:exact? x)))
    ) ;define

    (define (zero? x)
      (if (bigint? x) (bigint-zero? x) (base:zero? x))
    ) ;define

    (define (positive? x)
      (if (bigint? x) (bigint-positive? x) (base:positive? x))
    ) ;define

    (define (negative? x)
      (if (bigint? x) (bigint-negative? x) (base:negative? x))
    ) ;define

    (define (even? x)
      (if (bigint? x) (bigint-even? x) (base:even? x))
    ) ;define

    (define (odd? x)
      (if (bigint? x) (bigint-odd? x) (base:odd? x))
    ) ;define

    ;;; ---------- 转换与等价 ----------
    (define (exact x)
      (if (bigint? x) x (base:exact x))
    ) ;define

    (define (inexact x)
      (if (bigint? x) (bigint->inexact x) (base:inexact x))
    ) ;define

    (define exact->inexact inexact)
    (define inexact->exact exact)

    (define (number->string x . rest)
      (if (null? rest)
        (if (bigint? x) (bigint->string x) (base:number->string x))
        (if (bigint? x)
          (bigint->string x (car rest))
          (apply base:number->string x rest)
        ) ;if
      ) ;if
    ) ;define

    (define (string->number str . rest)
      (apply base:string->number str rest)
    ) ;define

    (define (eqv? a b)
      (cond ((and (bigint? a) (bigint? b)) (bigint=? a b))
            ((or (bigint? a) (bigint? b)) #f)
            (else (base:eqv? a b))
      ) ;cond
    ) ;define

    ;;; ---------- 比较 ----------
    (define (num-eq-2 a b)
      (cond
       ((and (exact-int? a) (exact-int? b))
        (if (or (bigint? a) (bigint? b))
          (bigint=? (to-bigint a) (to-bigint b))
          (base:= a b)
        ) ;if
       ) ;
       ((bigint? a)
        (if (and (base:rational? b) (base:exact? b))
          (bigint=? (bigint-multiply a (integer->bigint (base:denominator b)))
            (integer->bigint (base:numerator b))
          ) ;bigint=?
          (base:= (bigint->inexact a) b)
        ) ;if
       ) ;
       ((bigint? b)
        (if (and (base:rational? a) (base:exact? a))
          (bigint=? (integer->bigint (base:numerator a))
            (bigint-multiply b (integer->bigint (base:denominator a)))
          ) ;bigint=?
          (base:= a (bigint->inexact b))
        ) ;if
       ) ;
       (else (base:= a b))
      ) ;cond
    ) ;define

    (define (num-lt-2 a b)
      (cond
       ((and (exact-int? a) (exact-int? b))
        (if (or (bigint? a) (bigint? b))
          (bigint<? (to-bigint a) (to-bigint b))
          (base:< a b)
        ) ;if
       ) ;
       ((bigint? a)
        (if (and (base:rational? b) (base:exact? b))
          (bigint<? (bigint-multiply a (integer->bigint (base:denominator b)))
            (integer->bigint (base:numerator b))
          ) ;bigint<?
          (base:< (bigint->inexact a) b)
        ) ;if
       ) ;
       ((bigint? b)
        (if (and (base:rational? a) (base:exact? a))
          (bigint<? (integer->bigint (base:numerator a))
            (bigint-multiply b (integer->bigint (base:denominator a)))
          ) ;bigint<?
          (base:< a (bigint->inexact b))
        ) ;if
       ) ;
       (else (base:< a b))
      ) ;cond
    ) ;define

    (define (make-cmp-op cmp2)
      (lambda (a b . rest)
        (let loop
          ((x a) (y b) (r rest))
          (if (cmp2 x y) (if (null? r) #t (loop y (car r) (cdr r))) #f)
        ) ;let
      ) ;lambda
    ) ;define

    (define = (make-cmp-op num-eq-2))
    (define < (make-cmp-op num-lt-2))
    (define > (make-cmp-op (lambda (a b) (num-lt-2 b a))))
    (define <= (make-cmp-op (lambda (a b) (not (num-lt-2 b a)))))
    (define >= (make-cmp-op (lambda (a b) (not (num-lt-2 a b)))))

    (define (max first . rest)
      (let loop
        ((best first) (xs rest) (has-inexact (inexact? first)))
        (if (null? xs)
          (if (and has-inexact (exact? best)) (inexact best) best)
          (let* ((x (car xs)) (inx (or has-inexact (inexact? x))))
            (loop (if (num-lt-2 best x) x best) (cdr xs) inx)
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (min first . rest)
      (let loop
        ((best first) (xs rest) (has-inexact (inexact? first)))
        (if (null? xs)
          (if (and has-inexact (exact? best)) (inexact best) best)
          (let* ((x (car xs)) (inx (or has-inexact (inexact? x))))
            (loop (if (num-lt-2 x best) x best) (cdr xs) inx)
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    ;;; ---------- 注册 C 层读写钩子 ----------
    (when (defined? 's7-set-bignum-hooks!)
      (s7-set-bignum-hooks! '<bigint>
        (lambda (str radix) (guard (err (#t #f)) (string->bigint str radix)))
        bigint->string
      ) ;s7-set-bignum-hooks!
    ) ;when
  ) ;begin
) ;define-library
