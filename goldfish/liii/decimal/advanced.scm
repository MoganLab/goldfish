;;; liii/decimal/advanced.scm - decimal 高阶运算与 MathContext 上下文计算
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

(define-library (liii decimal advanced)
  (import (scheme base)
    (liii error)
    (liii bigint)
    (liii decimal record)
    (liii decimal math-context)
    (liii decimal compare)
    (liii decimal rounding)
    (liii decimal arithmetic)
    (liii decimal cast)
  ) ;import

  (export decimal-pow decimal-sqrt apply-math-context decimal-add-with-context
    decimal-subtract-with-context decimal-multiply-with-context
    decimal-divide-with-context decimal-pow-with-context
    decimal-sqrt-with-context decimal-abs-with-context
    decimal-negate-with-context decimal-plus-with-context
  ) ;export

  (begin
    (define (decimal-pow n exponent . args)
      (let* ((value (ensure-decimal "decimal-pow" n))
             (exp (if (not (integer? exponent))
                    (type-error "decimal-pow in (liii decimal advanced): exponent must be an integer"
                      exponent
                    ) ;type-error
                    (if (< exponent 0)
                      (value-error "decimal-pow in (liii decimal advanced): exponent must be non-negative"
                        exponent
                      ) ;value-error
                      exponent
                    ) ;if
                  ) ;if
             ) ;exp
            ) ;
        (cond ((= exp 0) (make-decimal-with-scale bigint-one 0))
              ((= exp 1) value)
              (else
                (let* ((result-scale (* (decimal-scale value) exp))
                       (result-unscaled (bigint-expt (decimal-unscaled value) exp))
                       (raw-result (make-decimal-with-scale result-unscaled result-scale))
                      ) ;
                  (if (null? args)
                    raw-result
                    (let ((target-scale (car args))
                          (rounding-mode (if (null? (cdr args)) decimal-round-unnecessary (cadr args)))
                         ) ;
                      (decimal-set-scale raw-result target-scale rounding-mode)
                    ) ;let
                  ) ;if
                ) ;let*
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    (define (decimal-sqrt n . args)
      (let ((value (ensure-decimal "decimal-sqrt" n)))
        (cond ((decimal-negative? value)
               (value-error "decimal-sqrt in (liii decimal advanced): cannot compute square root of negative number"
                 value
               ) ;value-error
              ) ;
              ((decimal-zero? value)
               (if (null? args) value (make-decimal-with-scale bigint-zero (car args)))
              ) ;
              (else
                (let* ((target-scale (if (null? args) 20 (car args)))
                       (rounding-mode
                         (if (or (null? args) (null? (cdr args))) decimal-round-half-up (cadr args))
                       ) ;rounding-mode
                       (working-scale (+ target-scale 4))
                      ) ;
                  (let loop
                    ((guess
                       (decimal-set-scale
                         (bigint->decimal (bigint-add (bigint-sqrt (decimal-to-bigint value)) bigint-one)
                         ) ;bigint->decimal
                         working-scale
                       ) ;decimal-set-scale
                     ) ;guess
                     (count 0)
                    ) ;
                    (let* ((quotient (decimal-divide value guess working-scale decimal-round-half-up))
                           (new-guess (decimal-divide (decimal-add guess quotient)
                                        (bigint->decimal (string->bigint "2"))
                                        working-scale
                                        decimal-round-half-up
                                      ) ;decimal-divide
                           ) ;new-guess
                          ) ;
                      (if (or (>= count 100)
                            (decimal=? guess new-guess)
                            (decimal=? (decimal-set-scale guess target-scale rounding-mode)
                              (decimal-set-scale new-guess target-scale rounding-mode)
                            ) ;decimal=?
                          ) ;or
                        (decimal-set-scale new-guess target-scale rounding-mode)
                        (loop new-guess (+ count 1))
                      ) ;if
                    ) ;let*
                  ) ;let
                ) ;let*
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (apply-math-context result mc)
      (if (math-context? mc) (decimal-round result mc) result)
    ) ;define

    (define (decimal-add-with-context a b mc)
      (apply-math-context (decimal-add a b) mc)
    ) ;define

    (define (decimal-subtract-with-context a b mc)
      (apply-math-context (decimal-subtract a b) mc)
    ) ;define

    (define (decimal-multiply-with-context a b mc)
      (apply-math-context (decimal-multiply a b) mc)
    ) ;define

    (define (decimal-divide-with-context a b mc)
      (let ((left (ensure-decimal "decimal-divide-with-context" a))
            (right (ensure-decimal "decimal-divide-with-context" b))
            (precision (math-context-precision mc))
            (rounding-mode (math-context-rounding-mode mc))
           ) ;
        (if (= precision 0)
          (decimal-divide left right 30 rounding-mode)
          (let* ((int-digits-a (- (decimal-precision left) (decimal-scale left)))
                 (int-digits-b (- (decimal-precision right) (decimal-scale right)))
                 (est (- int-digits-a int-digits-b))
                 (temp-scale (+ (max 0 (- precision est)) 10))
                 (raw-quotient (decimal-divide left right temp-scale rounding-mode))
                 (stripped (decimal-strip-trailing-zeros raw-quotient))
                 (pref-scale (- (decimal-scale left) (decimal-scale right)))
                ) ;
            (if (and (<= (decimal-precision stripped) precision)
                  (>= (decimal-scale stripped) pref-scale)
                ) ;and
              stripped
              (decimal-round raw-quotient mc)
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (decimal-pow-with-context n exponent mc)
      (let ((raw-result (decimal-pow n exponent)))
        (apply-math-context raw-result mc)
      ) ;let
    ) ;define

    (define (decimal-sqrt-with-context n mc)
      (let ((precision (math-context-precision mc))
            (rounding-mode (math-context-rounding-mode mc))
           ) ;
        (if (= precision 0)
          (decimal-sqrt n 20 rounding-mode)
          (decimal-sqrt n precision rounding-mode)
        ) ;if
      ) ;let
    ) ;define

    (define (decimal-abs-with-context n mc)
      (apply-math-context (decimal-abs n) mc)
    ) ;define

    (define (decimal-negate-with-context n mc)
      (apply-math-context (decimal-negate n) mc)
    ) ;define

    (define (decimal-plus-with-context n mc)
      (apply-math-context (decimal-plus n) mc)
    ) ;define

  ) ;begin
) ;define-library
