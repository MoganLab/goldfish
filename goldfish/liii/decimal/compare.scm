;;; liii/decimal/compare.scm - decimal 比较
;;;
;;; Copyright (c) 2026 Liii Network
;;; All Rights Reserved

(define-library (liii decimal compare)
  (import (scheme base) (liii error) (liii bigint) (liii decimal record))

  (export decimal=? decimal<? decimal>? decimal<=? decimal>=? decimal-hash-code)

  (begin
    (define (ensure-decimal who obj)
      (if (decimal? obj)
        obj
        (type-error (string-append who " in (liii decimal compare): not a decimal") obj)
      ) ;if
    ) ;define

    (define (power-of-ten exponent)
      (bigint-expt bigint-ten exponent)
    ) ;define

    (define (rescaled-unscaled value target-scale)
      (let* ((number (ensure-decimal "rescaled-unscaled" value))
             (current-scale (decimal-scale number))
            ) ;
        (if (= current-scale target-scale)
          (decimal-unscaled number)
          (bigint-multiply (decimal-unscaled number)
            (power-of-ten (- target-scale current-scale))
          ) ;bigint-multiply
        ) ;if
      ) ;let*
    ) ;define

    (define (align-decimals a b)
      (let* ((left (ensure-decimal "align-decimals" a))
             (right (ensure-decimal "align-decimals" b))
             (target-scale (max (decimal-scale left) (decimal-scale right)))
            ) ;
        (values (rescaled-unscaled left target-scale)
          (rescaled-unscaled right target-scale)
          target-scale
        ) ;values
      ) ;let*
    ) ;define

    (define (decimal-compare a b)
      (call-with-values (lambda () (align-decimals a b))
        (lambda (left right scale)
          (cond ((bigint<? left right) -1)
                ((bigint>? left right) 1)
                (else 0)
          ) ;cond
        ) ;lambda
      ) ;call-with-values
    ) ;define

    (define (decimal=? a b)
      (= (decimal-compare a b) 0)
    ) ;define

    (define (decimal<? a b)
      (= (decimal-compare a b) -1)
    ) ;define

    (define (decimal>? a b)
      (= (decimal-compare a b) 1)
    ) ;define

    (define (decimal<=? a b)
      (not (decimal>? a b))
    ) ;define

    (define (decimal>=? a b)
      (not (decimal<? a b))
    ) ;define

    ;; decimal-hash-code: 计算decimal的哈希码
    ;; 基于unscaled-value和scale的组合，折叠到 fixnum 域内
    (define (decimal-hash-code n)
      (let* ((value (ensure-decimal "decimal-hash-code" n))
             (unscaled (decimal-unscaled value))
             (scale (decimal-scale value))
             (str (bigint->string unscaled))
             (len (string-length str))
             (mod-p 2147483647)
            ) ;
        (let loop
          ((i 0) (h 0))
          (if (= i len)
            (modulo (+ (* h 31) (modulo scale mod-p)) mod-p)
            (let* ((c (string-ref str i)) (code (char->integer c)))
              (loop (+ i 1) (modulo (+ (* h 31) code) mod-p))
            ) ;let*
          ) ;if
        ) ;let
      ) ;let*
    ) ;define
  ) ;begin
) ;define-library
