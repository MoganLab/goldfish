;;; liii/decimal/rounding.scm - decimal 舍入模式与精度调节
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

(define-library (liii decimal rounding)
  (import (scheme base)
    (liii error)
    (liii bigint)
    (liii decimal record)
    (liii decimal math-context)
  ) ;import

  (export decimal-round-up decimal-round-down decimal-round-ceiling
    decimal-round-floor decimal-round-half-up decimal-round-half-down
    decimal-round-half-even decimal-round-unnecessary ensure-decimal
    ensure-rounding-mode power-of-ten decimal-precision round-away-from-zero
    apply-rounding decimal-set-scale decimal-strip-trailing-zeros decimal-floor
    decimal-ceiling decimal-truncate decimal-round
  ) ;export

  (begin
    (define decimal-round-up 'up)
    (define decimal-round-down 'down)
    (define decimal-round-ceiling 'ceiling)
    (define decimal-round-floor 'floor)
    (define decimal-round-half-up 'half-up)
    (define decimal-round-half-down 'half-down)
    (define decimal-round-half-even 'half-even)
    (define decimal-round-unnecessary 'unnecessary)

    (define (ensure-decimal who obj)
      (if (decimal? obj)
        obj
        (type-error (string-append who " in (liii decimal rounding): not a decimal")
          obj
        ) ;type-error
      ) ;if
    ) ;define

    (define (power-of-ten exponent)
      (bigint-expt bigint-ten (abs exponent))
    ) ;define

    (define (decimal-precision n)
      (let ((value (ensure-decimal "decimal-precision" n)))
        (if (bigint-zero? (decimal-unscaled value))
          1
          (string-length (bigint->string (bigint-abs (decimal-unscaled value))))
        ) ;if
      ) ;let
    ) ;define

    (define (ensure-rounding-mode who mode)
      (if (memq mode
            (list decimal-round-up decimal-round-down decimal-round-ceiling
              decimal-round-floor decimal-round-half-up decimal-round-half-down
              decimal-round-half-even decimal-round-unnecessary
            ) ;list
          ) ;memq
        mode
        (value-error (string-append who " in (liii decimal rounding): invalid rounding mode")
          mode
        ) ;value-error
      ) ;if
    ) ;define

    (define (round-away-from-zero quotient negative?)
      (if negative?
        (bigint-subtract quotient bigint-one)
        (bigint-add quotient bigint-one)
      ) ;if
    ) ;define

    (define (apply-rounding who quotient remainder divisor negative? mode)
      (if (bigint-zero? remainder)
        quotient
        (case mode
         ((down) quotient)
         ((up) (round-away-from-zero quotient negative?))
         ((ceiling) (if negative? quotient (bigint-add quotient bigint-one)))
         ((floor) (if negative? (bigint-subtract quotient bigint-one) quotient))
         ((unnecessary)
          (value-error (string-append who " in (liii decimal rounding): rounding necessary")
            remainder
          ) ;value-error
         ) ;
         (else
           (let* ((twice-remainder (bigint-multiply (bigint-abs remainder) bigint-two))
                  (abs-divisor (bigint-abs divisor))
                  (cmp (cond ((bigint<? twice-remainder abs-divisor) -1)
                             ((bigint>? twice-remainder abs-divisor) 1)
                             (else 0)
                       ) ;cond
                  ) ;cmp
                 ) ;
             (case mode
              ((half-up) (if (>= cmp 0) (round-away-from-zero quotient negative?) quotient))
              ((half-down) (if (> cmp 0) (round-away-from-zero quotient negative?) quotient))
              ((half-even)
               (if (> cmp 0)
                 (round-away-from-zero quotient negative?)
                 (if (< cmp 0)
                   quotient
                   (if (bigint-odd? quotient) (round-away-from-zero quotient negative?) quotient)
                 ) ;if
               ) ;if
              ) ;
              (else quotient)
             ) ;case
           ) ;let*
         ) ;else
        ) ;case
      ) ;if
    ) ;define

    (define (decimal-set-scale n scale . maybe-rounding-mode)
      (let* ((value (ensure-decimal "decimal-set-scale" n))
             (target-scale (if (not (integer? scale))
                             (type-error "decimal-set-scale in (liii decimal rounding): scale must be an integer"
                               scale
                             ) ;type-error
                             scale
                           ) ;if
             ) ;target-scale
             (current-scale (decimal-scale value))
            ) ;
        (if (= target-scale current-scale)
          value
          (if (> target-scale current-scale)
            (make-decimal-with-scale (bigint-multiply (decimal-unscaled value)
                                       (power-of-ten (- target-scale current-scale))
                                     ) ;bigint-multiply
              target-scale
            ) ;make-decimal-with-scale
            (let* ((rounding-mode (ensure-rounding-mode "decimal-set-scale"
                                    (if (null? maybe-rounding-mode)
                                      decimal-round-unnecessary
                                      (car maybe-rounding-mode)
                                    ) ;if
                                  ) ;ensure-rounding-mode
                   ) ;rounding-mode
                   (divisor (power-of-ten (- current-scale target-scale)))
                  ) ;
              (call-with-values (lambda () (bigint-divide-and-remainder (decimal-unscaled value) divisor))
                (lambda (quotient remainder)
                  (make-decimal-with-scale (apply-rounding "decimal-set-scale"
                                             quotient
                                             remainder
                                             divisor
                                             (bigint-negative? (decimal-unscaled value))
                                             rounding-mode
                                           ) ;apply-rounding
                    target-scale
                  ) ;make-decimal-with-scale
                ) ;lambda
              ) ;call-with-values
            ) ;let*
          ) ;if
        ) ;if
      ) ;let*
    ) ;define

    (define (decimal-strip-trailing-zeros n)
      (let* ((value (ensure-decimal "decimal-strip-trailing-zeros" n))
             (unscaled (decimal-unscaled value))
             (scale (decimal-scale value))
            ) ;
        (cond ((bigint-zero? unscaled) (make-decimal-with-scale bigint-zero 0))
              ((<= scale 0) value)
              (else
                (let loop
                  ((u unscaled) (s scale))
                  (if (and (> s 0) (bigint-zero? (bigint-remainder u bigint-ten)))
                    (loop (bigint-divide u bigint-ten) (- s 1))
                    (make-decimal-with-scale u s)
                  ) ;if
                ) ;let
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    (define (decimal-floor n)
      (decimal-set-scale n 0 decimal-round-floor)
    ) ;define

    (define (decimal-ceiling n)
      (decimal-set-scale n 0 decimal-round-ceiling)
    ) ;define

    (define (decimal-truncate n)
      (decimal-set-scale n 0 decimal-round-down)
    ) ;define

    (define (decimal-round n arg1 . rest)
      (let ((value (ensure-decimal "decimal-round" n)))
        (cond
         ((and (integer? arg1) (not (null? rest)))
          (let ((rounding-mode (car rest)))
            (ensure-rounding-mode "decimal-round" rounding-mode)
            (decimal-set-scale value arg1 rounding-mode)
          ) ;let
         ) ;
         ((math-context? arg1)
          (let ((precision (math-context-precision arg1))
                (rounding-mode (math-context-rounding-mode arg1))
               ) ;
            (if (or (= precision 0) (bigint-zero? (decimal-unscaled value)))
              value
              (let ((current-precision (decimal-precision value)))
                (if (<= current-precision precision)
                  value
                  (let* ((scale-adjustment (- precision current-precision))
                         (target-scale (+ (decimal-scale value) scale-adjustment))
                        ) ;
                    (decimal-set-scale value target-scale rounding-mode)
                  ) ;let*
                ) ;if
              ) ;let
            ) ;if
          ) ;let
         ) ;
         (else (type-error "decimal-round in (liii decimal rounding): invalid arguments, expected (n scale rounding-mode) or (n math-context)"
                 (cons arg1 rest)
               ) ;type-error
         ) ;else
        ) ;cond
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
