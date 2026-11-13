;;; liii/bignum/convert.scm - bignum 类型转换与精确性判定（由 liii/bignum.scm include）
;;;
;;; Copyright (c) 2026 Liii Network
;;; All Rights Reserved

(define INT64-MAX 9223372036854775807)

(define INT64-MIN -9223372036854775808)

(define INT64-MAX-BIG (string->bigint "9223372036854775807"))

(define INT64-MIN-BIG (string->bigint "-9223372036854775808"))

(define (bn:exact-int? x)
  (or (bigint? x) (and (native:integer? x) (native:exact? x)))
) ;define

(define (bn:to-bigint x)
  (if (bigint? x) x (integer->bigint x))
) ;define

(define (bn:demote-bigint b)
  (if (and (bigint>=? b INT64-MIN-BIG) (bigint<=? b INT64-MAX-BIG))
    (native:string->number (bigint->string b))
    b
  ) ;if
) ;define

(define (bn:finite-real? x)
  (and (native:real? x) (native:= (native:- x x) 0.0))
) ;define

(define (bn:inexact-large->bigint x)
  (let* ((neg? (native:< x 0.0)) (ax (if neg? (native:- x) x)))
    (let loop
      ((v ax) (shift 0))
      (if (native:< v 9007199254740992.0)
        (let* ((m (integer->bigint (native:exact v)))
               (res (bigint-multiply m (bigint-expt (integer->bigint 2) shift)))
              ) ;
          (bn:demote-bigint (if neg? (bigint-negate res) res))
        ) ;let*
        (loop (native:* v 0.5) (native:+ shift 1))
      ) ;if
    ) ;let
  ) ;let*
) ;define

(define (bn:bigint->inexact b)
  (native:string->number (string-append (bigint->string b) ".0"))
) ;define

(define (bn:any-int? x)
  (or (bigint? x)
    (and (bn:finite-real? x)
      (or (native:integer? x) (native:= x (native:truncate x)))
    ) ;and
  ) ;or
) ;define

(define (bn:any-int->bigint x)
  (cond ((bigint? x) x)
        ((native:exact? x) (integer->bigint x))
        ((not (native:< (native:abs x) 9007199254740992.0))
         (bn:to-bigint (bn:inexact-large->bigint x))
        ) ;
        (else (integer->bigint (native:exact x)))
  ) ;cond
) ;define

(define (bn:int-res->inexact b)
  (let ((d (bn:demote-bigint b)))
    (if (bigint? d) (bn:bigint->inexact d) (native:inexact d))
  ) ;let
) ;define

(define (bn:floor x)
  (if (bigint? x) x (native:floor x))
) ;define

(define (bn:ceiling x)
  (if (bigint? x) x (native:ceiling x))
) ;define

(define (bn:truncate x)
  (if (bigint? x) x (native:truncate x))
) ;define

(define (bn:round x)
  (if (bigint? x) x (native:round x))
) ;define

(define (bn:numerator x)
  (if (bigint? x) x (native:numerator x))
) ;define

(define (bn:denominator x)
  (if (bigint? x) 1 (native:denominator x))
) ;define
