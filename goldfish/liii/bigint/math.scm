;;; liii/bigint/math.scm - bigint 数论与高阶运算（GCD、模逆、方根等）
;;;
;;; Copyright (c) 2024-2026 Liii Network
;;; All Rights Reserved

(define-library (liii bigint math)
  (import (scheme base)
    (liii error)
    (liii bigint record)
    (liii bigint constant)
    (liii bigint compare)
    (liii bigint predicate)
    (liii bigint string)
    (liii bigint arithmetic)
  ) ;import

  (export bigint-gcd bigint-mod bigint-mod-expt bigint-mod-inverse bigint-sqrt
    bigint-sqrt-and-remainder bigint-rootn bigint-rootn-and-remainder
  ) ;export

  (begin
    (define (bigint-gcd a b)
      (let loop
        ((x (bigint-abs (ensure-bigint "bigint-gcd" a)))
         (y (bigint-abs (ensure-bigint "bigint-gcd" b)))
        ) ;
        (if (bigint-zero? y) x (loop y (bigint-remainder x y)))
      ) ;let
    ) ;define

    ;;; ---------- 模运算 ----------
    ;; 返回与 a 模 m 同余的非负 bigint，结果始终在 [0, m-1] 范围内
    ;; 如果 m 不是正数，则抛出错误
    (define (bigint-mod a m)
      (let* ((value (ensure-bigint "bigint-mod" a))
             (modulus (ensure-bigint "bigint-mod" m))
             (signum-m (bigint-signum modulus))
            ) ;
        (if (not (= signum-m 1))
          (value-error "bigint-mod in (liii bigint math): modulus must be positive" m)
          (let ((remainder (bigint-remainder value modulus)))
            (if (bigint-negative? remainder) (bigint-add remainder modulus) remainder)
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    ;; 返回 base^exponent mod m
    (define (bigint-mod-expt base exponent m)
      (let* ((b (ensure-bigint "bigint-mod-expt" base))
             (e (ensure-bigint "bigint-mod-expt" exponent))
             (modulus (ensure-bigint "bigint-mod-expt" m))
            ) ;
        (cond
         ((not (= (bigint-signum modulus) 1))
          (value-error "bigint-mod-expt in (liii bigint math): modulus must be positive"
            m
          ) ;value-error
         ) ;
         ((bigint-negative? e)
          (bigint-mod-expt (bigint-mod-inverse b modulus) (bigint-negate e) modulus)
         ) ;
         (else
           (let loop
             ((acc (bigint-mod (integer->bigint 1) modulus))
              (factor (bigint-mod b modulus))
              (power e)
             ) ;
             (if (bigint-zero? power)
               acc
               (let ((next-acc (if (bigint-even-internal? power)
                                 acc
                                 (bigint-mod (bigint-multiply acc factor) modulus)
                               ) ;if
                     ) ;next-acc
                    ) ;
                 (loop next-acc
                   (bigint-mod (bigint-multiply factor factor) modulus)
                   (bigint-divide power bigint-two)
                 ) ;loop
               ) ;let
             ) ;if
           ) ;let
         ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    ;; 扩展欧几里得算法：返回 (g, x, y) 使得 a*x + b*y = g = gcd(a, b)
    (define (extended-gcd a b)
      (if (bigint-zero? b)
        (values a (integer->bigint 1) (integer->bigint 0))
        (call-with-values (lambda () (extended-gcd b (bigint-remainder a b)))
          (lambda (g x1 y1)
            (let ((x y1) (y (bigint-subtract x1 (bigint-multiply y1 (bigint-divide a b)))))
              (values g x y)
            ) ;let
          ) ;lambda
        ) ;call-with-values
      ) ;if
    ) ;define

    ;; 返回 a 模 m 的模乘法逆元
    (define (bigint-mod-inverse a m)
      (let* ((value (ensure-bigint "bigint-mod-inverse" a))
             (modulus (ensure-bigint "bigint-mod-inverse" m))
            ) ;
        (cond
         ((not (= (bigint-signum modulus) 1))
          (value-error "bigint-mod-inverse in (liii bigint math): modulus must be positive"
            m
          ) ;value-error
         ) ;
         ((bigint-zero? modulus)
          (value-error "bigint-mod-inverse in (liii bigint math): modulus cannot be zero")
         ) ;
         (else
           (let* ((a-mod (bigint-mod value modulus)) (g (bigint-gcd a-mod modulus)))
             (if (not (bigint=? g (integer->bigint 1)))
               (value-error "bigint-mod-inverse in (liii bigint math): a and m are not coprime"
                 a
                 m
               ) ;value-error
               (call-with-values (lambda () (extended-gcd a-mod modulus))
                 (lambda (g x y) (bigint-mod x modulus))
               ) ;call-with-values
             ) ;if
           ) ;let*
         ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    (define (bigint-sqrt-and-remainder n)
      (let ((value (ensure-bigint "bigint-sqrt-and-remainder" n)))
        (cond ((bigint-negative? value)
               (value-error "bigint-sqrt-and-remainder in (liii bigint math): square root of negative bigint"
                 value
               ) ;value-error
              ) ;
              ((bigint-zero? value) (values bigint-zero bigint-zero))
              ((bigint=? value bigint-one) (values bigint-one bigint-zero))
              (else
                (let loop
                  ((low bigint-one) (high value) (best bigint-zero))
                  (if (bigint>? low high)
                    (let ((remainder (bigint-subtract value (bigint-multiply best best))))
                      (values best remainder)
                    ) ;let
                    (let* ((mid (bigint-divide (bigint-add low high) bigint-two))
                           (square (bigint-multiply mid mid))
                          ) ;
                      (cond ((bigint=? square value) (values mid bigint-zero))
                            ((bigint<? square value) (loop (bigint-add mid bigint-one) high mid))
                            (else (loop low (bigint-subtract mid bigint-one) best))
                      ) ;cond
                    ) ;let*
                  ) ;if
                ) ;let
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (bigint-sqrt n)
      (call-with-values (lambda () (bigint-sqrt-and-remainder n))
        (lambda (root remainder) root)
      ) ;call-with-values
    ) ;define

    (define (normalize-root-degree who degree)
      (if (not (integer? degree))
        (type-error (string-append who " in (liii bigint math): root degree must be an integer")
          degree
        ) ;type-error
        (if (<= degree 0)
          (value-error (string-append who " in (liii bigint math): root degree must be positive")
            degree
          ) ;value-error
          degree
        ) ;if
      ) ;if
    ) ;define

    (define (positive-bigint-rootn-and-remainder value degree)
      (cond ((= degree 1) (values value bigint-zero))
            ((= degree 2) (bigint-sqrt-and-remainder value))
            ((bigint-zero? value) (values bigint-zero bigint-zero))
            (else
              (let loop
                ((low bigint-one) (high value) (best bigint-zero))
                (if (bigint>? low high)
                  (let ((remainder (bigint-subtract value (bigint-expt best degree))))
                    (values best remainder)
                  ) ;let
                  (let* ((mid (bigint-divide (bigint-add low high) bigint-two))
                         (power (bigint-expt mid degree))
                        ) ;
                    (cond ((bigint=? power value) (values mid bigint-zero))
                          ((bigint<? power value) (loop (bigint-add mid bigint-one) high mid))
                          (else (loop low (bigint-subtract mid bigint-one) best))
                    ) ;cond
                  ) ;let*
                ) ;if
              ) ;let
            ) ;else
      ) ;cond
    ) ;define

    (define (bigint-rootn-and-remainder n degree)
      (let* ((value (ensure-bigint "bigint-rootn-and-remainder" n))
             (root-degree (normalize-root-degree "bigint-rootn-and-remainder" degree))
            ) ;
        (cond
         ((bigint-negative? value)
          (if (even? root-degree)
            (value-error "bigint-rootn-and-remainder in (liii bigint math): negative bigint with even root degree"
              value
              degree
            ) ;value-error
            (call-with-values (lambda () (positive-bigint-rootn-and-remainder (bigint-abs value) root-degree))
              (lambda (root remainder)
                (values (bigint-negate root) (bigint-negate remainder))
              ) ;lambda
            ) ;call-with-values
          ) ;if
         ) ;
         (else (positive-bigint-rootn-and-remainder value root-degree))
        ) ;cond
      ) ;let*
    ) ;define

    (define (bigint-rootn n degree)
      (call-with-values (lambda () (bigint-rootn-and-remainder n degree))
        (lambda (root remainder) root)
      ) ;call-with-values
    ) ;define

  ) ;begin
) ;define-library
