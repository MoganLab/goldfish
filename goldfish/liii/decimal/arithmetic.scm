;;; liii/decimal/arithmetic.scm - decimal 基础算术与属性计算
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

(define-library (liii decimal arithmetic)
  (import (scheme base)
    (liii error)
    (liii bigint)
    (liii decimal record)
    (liii decimal compare)
    (liii decimal rounding)
  ) ;import

  (export decimal-unscaled-value decimal-signum decimal-ulp rescaled-unscaled
    align-decimals decimal-add decimal-subtract decimal-multiply decimal-divide
    decimal-move-point-left decimal-scale-by-power-of-ten
    decimal-move-point-right decimal-divide-to-integral-value decimal-remainder
    decimal-divide-and-remainder decimal-plus decimal-exact-integer? decimal-max
    decimal-min decimal-negate decimal-abs decimal-zero? decimal-positive?
    decimal-negative? decimal-equals?
  ) ;export

  (begin
    (define (decimal-unscaled-value n)
      (decimal-unscaled (ensure-decimal "decimal-unscaled-value" n))
    ) ;define

    (define (decimal-signum n)
      (bigint-signum (decimal-unscaled (ensure-decimal "decimal-signum" n)))
    ) ;define

    (define (decimal-ulp n)
      (let ((value (ensure-decimal "decimal-ulp" n)))
        (make-decimal-with-scale bigint-one (decimal-scale value))
      ) ;let
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

    (define (decimal-add a b)
      (call-with-values (lambda () (align-decimals a b))
        (lambda (left right scale)
          (make-decimal-with-scale (bigint-add left right) scale)
        ) ;lambda
      ) ;call-with-values
    ) ;define

    (define (decimal-subtract a b)
      (call-with-values (lambda () (align-decimals a b))
        (lambda (left right scale)
          (make-decimal-with-scale (bigint-subtract left right) scale)
        ) ;lambda
      ) ;call-with-values
    ) ;define

    (define (decimal-multiply a b)
      (let* ((left (ensure-decimal "decimal-multiply" a))
             (right (ensure-decimal "decimal-multiply" b))
            ) ;
        (make-decimal-with-scale (bigint-multiply (decimal-unscaled left) (decimal-unscaled right))
          (+ (decimal-scale left) (decimal-scale right))
        ) ;make-decimal-with-scale
      ) ;let*
    ) ;define

    (define (decimal-divide a b scale . maybe-rounding-mode)
      (let* ((left (ensure-decimal "decimal-divide" a))
             (right (ensure-decimal "decimal-divide" b))
             (target-scale (if (not (integer? scale))
                             (type-error "decimal-divide in (liii decimal arithmetic): scale must be an integer"
                               scale
                             ) ;type-error
                             scale
                           ) ;if
             ) ;target-scale
             (rounding-mode (ensure-rounding-mode "decimal-divide"
                              (if (null? maybe-rounding-mode)
                                decimal-round-unnecessary
                                (car maybe-rounding-mode)
                              ) ;if
                            ) ;ensure-rounding-mode
             ) ;rounding-mode
             (divisor-unscaled (decimal-unscaled right))
            ) ;
        (if (bigint-zero? divisor-unscaled)
          (value-error "decimal-divide in (liii decimal arithmetic): division by zero" b)
          (let* ((scale-adjustment (+ target-scale (decimal-scale right) (- (decimal-scale left)))
                 ) ;scale-adjustment
                 (dividend (if (>= scale-adjustment 0)
                             (bigint-multiply (decimal-unscaled left) (power-of-ten scale-adjustment))
                             (decimal-unscaled left)
                           ) ;if
                 ) ;dividend
                 (actual-divisor
                   (if (>= scale-adjustment 0)
                     divisor-unscaled
                     (bigint-multiply divisor-unscaled (power-of-ten (- scale-adjustment)))
                   ) ;if
                 ) ;actual-divisor
                ) ;
            (call-with-values (lambda () (bigint-divide-and-remainder dividend actual-divisor))
              (lambda (quotient remainder)
                (make-decimal-with-scale
                  (apply-rounding "decimal-divide"
                    quotient
                    remainder
                    actual-divisor
                    (not (= (* (bigint-signum dividend) (bigint-signum actual-divisor)) 1))
                    rounding-mode
                  ) ;apply-rounding
                  target-scale
                ) ;make-decimal-with-scale
              ) ;lambda
            ) ;call-with-values
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    (define (decimal-move-point-left n shift)
      (let ((value (ensure-decimal "decimal-move-point-left" n)))
        (if (not (integer? shift))
          (type-error "decimal-move-point-left in (liii decimal arithmetic): shift must be an integer"
            shift
          ) ;type-error
          (if (negative? shift)
            (decimal-move-point-right value (- shift))
            (make-decimal-with-scale (decimal-unscaled value)
              (+ (decimal-scale value) shift)
            ) ;make-decimal-with-scale
          ) ;if
        ) ;if
      ) ;let
    ) ;define

    (define (decimal-scale-by-power-of-ten n shift)
      (let ((value (ensure-decimal "decimal-scale-by-power-of-ten" n)))
        (if (not (integer? shift))
          (type-error "decimal-scale-by-power-of-ten in (liii decimal arithmetic): shift must be an integer"
            shift
          ) ;type-error
          (decimal-move-point-right value shift)
        ) ;if
      ) ;let
    ) ;define

    (define (decimal-move-point-right n shift)
      (let ((value (ensure-decimal "decimal-move-point-right" n)))
        (if (not (integer? shift))
          (type-error "decimal-move-point-right in (liii decimal arithmetic): shift must be an integer"
            shift
          ) ;type-error
          (if (negative? shift)
            (decimal-move-point-left value (- shift))
            (let ((current-scale (decimal-scale value)))
              (if (<= shift current-scale)
                (make-decimal-with-scale (decimal-unscaled value) (- current-scale shift))
                (make-decimal-with-scale (bigint-multiply (decimal-unscaled value)
                                           (power-of-ten (- shift current-scale))
                                         ) ;bigint-multiply
                  0
                ) ;make-decimal-with-scale
              ) ;if
            ) ;let
          ) ;if
        ) ;if
      ) ;let
    ) ;define

    (define (decimal-divide-to-integral-value a b)
      (let* ((left (ensure-decimal "decimal-divide-to-integral-value" a))
             (right (ensure-decimal "decimal-divide-to-integral-value" b))
             (divisor-unscaled (decimal-unscaled right))
            ) ;
        (if (bigint-zero? divisor-unscaled)
          (value-error "decimal-divide-to-integral-value in (liii decimal arithmetic): division by zero"
            b
          ) ;value-error
          (let* ((scale-adjustment (- (decimal-scale right) (decimal-scale left)))
                 (scaled-dividend
                   (if (>= scale-adjustment 0)
                     (bigint-multiply (decimal-unscaled left) (power-of-ten scale-adjustment))
                     (bigint-divide (decimal-unscaled left) (power-of-ten (- scale-adjustment)))
                   ) ;if
                 ) ;scaled-dividend
                ) ;
            (call-with-values (lambda () (bigint-divide-and-remainder scaled-dividend divisor-unscaled))
              (lambda (quotient remainder) (make-decimal-with-scale quotient 0))
            ) ;call-with-values
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    (define (decimal-remainder a b)
      (call-with-values (lambda () (decimal-divide-and-remainder a b))
        (lambda (quotient remainder) remainder)
      ) ;call-with-values
    ) ;define

    (define (decimal-divide-and-remainder a b)
      (let* ((left (ensure-decimal "decimal-divide-and-remainder" a))
             (right (ensure-decimal "decimal-divide-and-remainder" b))
             (quotient (decimal-divide-to-integral-value left right))
             (remainder (decimal-subtract left (decimal-multiply quotient right)))
            ) ;
        (values quotient remainder)
      ) ;let*
    ) ;define

    (define (decimal-plus n)
      (ensure-decimal "decimal-plus" n)
    ) ;define

    (define (decimal-exact-integer? n)
      (let* ((value (ensure-decimal "decimal-exact-integer?" n))
             (scale (decimal-scale value))
            ) ;
        (or (<= scale 0)
          (bigint-zero? (bigint-remainder (decimal-unscaled value) (power-of-ten scale)))
        ) ;or
      ) ;let*
    ) ;define

    (define (decimal-max a b)
      (if (decimal<? a b) b a)
    ) ;define

    (define (decimal-min a b)
      (if (decimal<? a b) a b)
    ) ;define

    (define (decimal-negate n)
      (let ((value (ensure-decimal "decimal-negate" n)))
        (make-decimal-with-scale (bigint-negate (decimal-unscaled value))
          (decimal-scale value)
        ) ;make-decimal-with-scale
      ) ;let
    ) ;define

    (define (decimal-abs n)
      (let ((value (ensure-decimal "decimal-abs" n)))
        (make-decimal-with-scale (bigint-abs (decimal-unscaled value))
          (decimal-scale value)
        ) ;make-decimal-with-scale
      ) ;let
    ) ;define

    (define (decimal-zero? n)
      (bigint-zero? (decimal-unscaled (ensure-decimal "decimal-zero?" n)))
    ) ;define

    (define (decimal-positive? n)
      (bigint-positive? (decimal-unscaled (ensure-decimal "decimal-positive?" n)))
    ) ;define

    (define (decimal-negative? n)
      (bigint-negative? (decimal-unscaled (ensure-decimal "decimal-negative?" n)))
    ) ;define

    (define (decimal-equals? a b)
      (let ((left (ensure-decimal "decimal-equals?" a))
            (right (ensure-decimal "decimal-equals?" b))
           ) ;
        (and (= (decimal-scale left) (decimal-scale right))
          (bigint=? (decimal-unscaled left) (decimal-unscaled right))
        ) ;and
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
