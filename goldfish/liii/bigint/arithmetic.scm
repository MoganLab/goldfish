;;; liii/bigint/arithmetic.scm - bigint 基本算术运算
;;;
;;; Copyright (c) 2024-2026 Liii Network
;;; All Rights Reserved

(define-library (liii bigint arithmetic)
  (import (scheme base)
    (liii error)
    (liii bigint record)
    (liii bigint constant)
    (liii bigint predicate)
    (liii bigint mag)
    (liii bigint multiply)
    (liii bigint division)
    (liii bigint string)
  ) ;import

  (export ensure-bigint bigint-even-internal? bigint-negate bigint-abs
    bigint-add bigint-subtract bigint-multiply bigint-divide-and-remainder
    bigint-divide bigint-quotient bigint-remainder bigint-modulo
    normalize-exponent bigint-expt
  ) ;export

  (begin
    (define (ensure-bigint who obj)
      (if (bigint? obj)
        obj
        (type-error (string-append who " in (liii bigint arithmetic): not a bigint")
          obj
        ) ;type-error
      ) ;if
    ) ;define

    (define (bigint-even-internal? n)
      (let ((mag (bigint-mag n)))
        (or (null? mag) (= (remainder (car (reverse mag)) 2) 0))
      ) ;let
    ) ;define

    (define (bigint-negate n)
      (let* ((value (ensure-bigint "bigint-negate" n)) (signum (bigint-signum value)))
        (cond ((= signum 0) value)
              ((= signum 1) (make-bigint -1 (bigint-mag value)))
              (else (make-bigint 1 (bigint-mag value)))
        ) ;cond
      ) ;let*
    ) ;define

    (define (bigint-abs n)
      (let ((value (ensure-bigint "bigint-abs" n)))
        (if (= (bigint-signum value) -1) (bigint-negate value) value)
      ) ;let
    ) ;define

    (define (bigint-add a b)
      (let* ((left (ensure-bigint "bigint-add" a))
             (right (ensure-bigint "bigint-add" b))
             (signum-a (bigint-signum left))
             (signum-b (bigint-signum right))
            ) ;
        (cond ((= signum-a 0) right)
              ((= signum-b 0) left)
              ((= signum-a signum-b)
               (make-bigint signum-a (add-mag (bigint-mag left) (bigint-mag right)))
              ) ;
              ((= signum-a 1)
               (let ((cmp (compare-mag (bigint-mag left) (bigint-mag right))))
                 (cond
                  ((> cmp 0) (make-bigint 1 (sub-mag (bigint-mag left) (bigint-mag right))))
                  ((< cmp 0) (make-bigint -1 (sub-mag (bigint-mag right) (bigint-mag left))))
                  (else bigint-zero)
                 ) ;cond
               ) ;let
              ) ;
              (else
                (let ((cmp (compare-mag (bigint-mag right) (bigint-mag left))))
                  (cond
                   ((> cmp 0) (make-bigint 1 (sub-mag (bigint-mag right) (bigint-mag left))))
                   ((< cmp 0) (make-bigint -1 (sub-mag (bigint-mag left) (bigint-mag right))))
                   (else bigint-zero)
                  ) ;cond
                ) ;let
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    (define (bigint-subtract a b)
      (bigint-add a (bigint-negate b))
    ) ;define

    (define (bigint-multiply a b)
      (let* ((left (ensure-bigint "bigint-multiply" a))
             (right (ensure-bigint "bigint-multiply" b))
             (signum-a (bigint-signum left))
             (signum-b (bigint-signum right))
            ) ;
        (cond ((or (= signum-a 0) (= signum-b 0)) bigint-zero)
              ((= signum-a signum-b)
               (make-bigint 1 (multiply-mag (bigint-mag left) (bigint-mag right)))
              ) ;
              (else (make-bigint -1 (multiply-mag (bigint-mag left) (bigint-mag right))))
        ) ;cond
      ) ;let*
    ) ;define

    (define (bigint-divide-and-remainder a b)
      (let* ((left (ensure-bigint "bigint-divide-and-remainder" a))
             (right (ensure-bigint "bigint-divide-and-remainder" b))
             (signum-a (bigint-signum left))
             (signum-b (bigint-signum right))
            ) ;
        (cond ((= signum-b 0)
               (value-error "bigint-divide-and-remainder in (liii bigint arithmetic): division by zero"
               ) ;value-error
              ) ;
              ((= signum-a 0) (values bigint-zero bigint-zero))
              (else
                (call-with-values (lambda () (positive-divide-and-remainder (bigint-abs left) (bigint-abs right)))
                  (lambda (quotient remainder)
                    (let ((signed-quotient (if (or (bigint-zero? quotient) (= signum-a signum-b))
                                             quotient
                                             (bigint-negate quotient)
                                           ) ;if
                          ) ;signed-quotient
                          (signed-remainder (if (or (bigint-zero? remainder) (= signum-a 1))
                                              remainder
                                              (bigint-negate remainder)
                                            ) ;if
                          ) ;signed-remainder
                         ) ;
                      (values signed-quotient signed-remainder)
                    ) ;let
                  ) ;lambda
                ) ;call-with-values
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    (define (bigint-divide a b)
      (call-with-values (lambda () (bigint-divide-and-remainder a b))
        (lambda (quotient remainder) quotient)
      ) ;call-with-values
    ) ;define

    (define bigint-quotient bigint-divide)

    (define (bigint-remainder a b)
      (call-with-values (lambda () (bigint-divide-and-remainder a b))
        (lambda (quotient remainder) remainder)
      ) ;call-with-values
    ) ;define

    (define (bigint-modulo a b)
      (let ((divisor (ensure-bigint "bigint-modulo" b)))
        (if (bigint-zero? divisor)
          (value-error "bigint-modulo in (liii bigint arithmetic): division by zero")
          (let ((remainder (bigint-remainder a divisor)))
            (if (or (bigint-zero? remainder) (bigint-positive? remainder))
              remainder
              (bigint-add remainder (bigint-abs divisor))
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (normalize-exponent exponent)
      (cond ((bigint? exponent)
             (if (bigint-negative? exponent)
               (value-error "bigint-expt in (liii bigint arithmetic): exponent must be non-negative"
                 exponent
               ) ;value-error
               exponent
             ) ;if
            ) ;
            ((integer? exponent)
             (if (< exponent 0)
               (value-error "bigint-expt in (liii bigint arithmetic): exponent must be non-negative"
                 exponent
               ) ;value-error
               (integer->bigint exponent)
             ) ;if
            ) ;
            (else (type-error "bigint-expt in (liii bigint arithmetic): exponent must be a non-negative integer"
                    exponent
                  ) ;type-error
            ) ;else
      ) ;cond
    ) ;define

    (define (bigint-expt base exponent)
      (let loop
        ((acc bigint-one)
         (factor (ensure-bigint "bigint-expt" base))
         (power (normalize-exponent exponent))
        ) ;
        (if (bigint-zero? power)
          acc
          (let ((next-acc (if (bigint-even-internal? power) acc (bigint-multiply acc factor))))
            (loop next-acc (bigint-multiply factor factor) (bigint-divide power bigint-two))
          ) ;let
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
