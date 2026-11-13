;;; liii/decimal/cast.scm - decimal 与其它数值类型互相转换
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

(define-library (liii decimal cast)
  (import (scheme base)
    (liii error)
    (liii bigint)
    (liii decimal record)
    (liii decimal string)
    (liii decimal rounding)
    (liii decimal arithmetic)
  ) ;import

  (export decimal-to-bigint decimal-to-bigint-exact bigint->decimal
    integer->decimal long->decimal double->decimal rational->decimal
    decimal->float
  ) ;export

  (begin
    (define (decimal-to-bigint n)
      (let* ((value (ensure-decimal "decimal-to-bigint" n))
             (scale (decimal-scale value))
             (unscaled (decimal-unscaled value))
            ) ;
        (if (<= scale 0)
          (bigint-multiply unscaled (power-of-ten (- scale)))
          (bigint-divide unscaled (power-of-ten scale))
        ) ;if
      ) ;let*
    ) ;define

    (define (decimal-to-bigint-exact n)
      (let* ((value (ensure-decimal "decimal-to-bigint-exact" n))
             (scale (decimal-scale value))
             (unscaled (decimal-unscaled value))
            ) ;
        (if (<= scale 0)
          (bigint-multiply unscaled (power-of-ten (- scale)))
          (call-with-values (lambda () (bigint-divide-and-remainder unscaled (power-of-ten scale)))
            (lambda (quotient remainder)
              (if (bigint-zero? remainder)
                quotient
                (value-error "decimal-to-bigint-exact in (liii decimal cast): decimal has fractional part"
                  n
                ) ;value-error
              ) ;if
            ) ;lambda
          ) ;call-with-values
        ) ;if
      ) ;let*
    ) ;define

    (define (bigint->decimal n . maybe-scale)
      (let* ((scale
               (if (null? maybe-scale)
                 0
                 (let ((s (car maybe-scale)))
                   (if (not (integer? s))
                     (type-error "bigint->decimal in (liii decimal cast): scale must be an integer"
                       s
                     ) ;type-error
                     s
                   ) ;if
                 ) ;let
               ) ;if
             ) ;scale
            ) ;
        (if (bigint? n)
          (make-decimal-with-scale n scale)
          (type-error "bigint->decimal in (liii decimal cast): not a bigint" n)
        ) ;if
      ) ;let*
    ) ;define

    (define (integer->decimal n . maybe-scale)
      (let* ((scale
               (if (null? maybe-scale)
                 0
                 (let ((s (car maybe-scale)))
                   (if (not (integer? s))
                     (type-error "integer->decimal in (liii decimal cast): scale must be an integer"
                       s
                     ) ;type-error
                     s
                   ) ;if
                 ) ;let
               ) ;if
             ) ;scale
            ) ;
        (if (integer? n)
          (make-decimal-with-scale (integer->bigint n) scale)
          (type-error "integer->decimal in (liii decimal cast): not an integer" n)
        ) ;if
      ) ;let*
    ) ;define

    (define (long->decimal n . maybe-scale)
      (apply integer->decimal n maybe-scale)
    ) ;define

    (define (double->decimal n . maybe-scale)
      (if (not (real? n))
        (type-error "double->decimal in (liii decimal cast): not a real number" n)
        (let* ((scale
                 (if (null? maybe-scale)
                   #f
                   (let ((s (car maybe-scale)))
                     (if (not (integer? s))
                       (type-error "double->decimal in (liii decimal cast): scale must be an integer"
                         s
                       ) ;type-error
                       s
                     ) ;if
                   ) ;let
                 ) ;if
               ) ;scale
               (str (number->string n))
               (d (string->decimal str))
              ) ;
          (if scale (decimal-set-scale d scale decimal-round-half-up) d)
        ) ;let*
      ) ;if
    ) ;define

    (define (rational->decimal numerator denominator . rest)
      (if (not (integer? numerator))
        (type-error "rational->decimal in (liii decimal cast): numerator must be an integer"
          numerator
        ) ;type-error
        (if (not (integer? denominator))
          (type-error "rational->decimal in (liii decimal cast): denominator must be an integer"
            denominator
          ) ;type-error
          (if (= denominator 0)
            (value-error "rational->decimal in (liii decimal cast): denominator cannot be zero"
              denominator
            ) ;value-error
            (let* ((scale
                     (if (null? rest)
                       0
                       (let ((s (car rest)))
                         (if (not (integer? s))
                           (type-error "rational->decimal in (liii decimal cast): scale must be an integer"
                             s
                           ) ;type-error
                           s
                         ) ;if
                       ) ;let
                     ) ;if
                   ) ;scale
                   (rounding-mode
                     (if (or (null? rest) (null? (cdr rest))) decimal-round-half-up (cadr rest))
                   ) ;rounding-mode
                   (num-decimal (integer->decimal numerator))
                   (den-decimal (integer->decimal denominator))
                  ) ;
              (decimal-divide num-decimal den-decimal scale rounding-mode)
            ) ;let*
          ) ;if
        ) ;if
      ) ;if
    ) ;define

    (define (decimal->float n)
      (let ((value (ensure-decimal "decimal->float" n)))
        (exact->inexact (string->number (decimal->string value)))
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
