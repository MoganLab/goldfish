;;; liii/decimal/type.scm - decimal 目标类型转换
;;;
;;; Copyright (c) 2026 Liii Network
;;; All Rights Reserved

(define-library (liii decimal type)
  (import (scheme base)
    (liii error)
    (liii bigint)
    (liii decimal record)
    (liii decimal string)
  ) ;import

  (export decimal->long decimal->long-exact decimal->int decimal->int-exact
    decimal->short decimal->short-exact decimal->byte decimal->byte-exact
    decimal->double decimal->float decimal->rational
  ) ;export

  (begin
    ;; decimal->long: 将decimal转换为long（带舍入）
    (define (decimal->long n)
      (let* ((value (ensure-decimal "decimal->long" n))
             (scale (decimal-scale value))
             (unscaled (decimal-unscaled value))
             (int-part
               (if (<= scale 0)
                 (bigint-multiply unscaled (bigint-expt (integer->bigint 10) (- scale)))
                 (bigint-divide unscaled (bigint-expt (integer->bigint 10) scale))
               ) ;if
             ) ;int-part
            ) ;
        (string->number (bigint->string int-part))
      ) ;let*
    ) ;define

    ;; decimal->long-exact: 精确转换，如果丢失信息则报错
    (define (decimal->long-exact n)
      (let ((value (ensure-decimal "decimal->long-exact" n)))
        (if (not (= (decimal-scale value) 0))
          (value-error "decimal->long-exact in (liii decimal type): decimal has fractional part"
            n
          ) ;value-error
          (string->number (bigint->string (decimal-unscaled value)))
        ) ;if
      ) ;let
    ) ;define

    ;; decimal->int: 将decimal转换为int（带舍入）
    (define (decimal->int n)
      (let ((long-val (decimal->long n)))
        (if (or (> long-val 2147483647) (< long-val -2147483648))
          (value-error "decimal->int in (liii decimal type): value out of int range" n)
          long-val
        ) ;if
      ) ;let
    ) ;define

    ;; decimal->int-exact: 精确转换，如果丢失信息则报错
    (define (decimal->int-exact n)
      (let ((value (ensure-decimal "decimal->int-exact" n)))
        (if (not (= (decimal-scale value) 0))
          (value-error "decimal->int-exact in (liii decimal type): decimal has fractional part"
            n
          ) ;value-error
          (let ((unscaled (decimal-unscaled value)))
            (if (or (bigint>? unscaled (integer->bigint 2147483647))
                  (bigint<? unscaled (integer->bigint -2147483648))
                ) ;or
              (value-error "decimal->int-exact in (liii decimal type): value out of int range"
                n
              ) ;value-error
              unscaled
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; decimal->short: 将decimal转换为short（带舍入）
    (define (decimal->short n)
      (let ((int-val (decimal->int n)))
        (if (or (> int-val 32767) (< int-val -32768))
          (value-error "decimal->short in (liii decimal type): value out of short range"
            n
          ) ;value-error
          int-val
        ) ;if
      ) ;let
    ) ;define

    ;; decimal->short-exact: 精确转换，如果丢失信息则报错
    (define (decimal->short-exact n)
      (let ((value (ensure-decimal "decimal->short-exact" n)))
        (if (not (= (decimal-scale value) 0))
          (value-error "decimal->short-exact in (liii decimal type): decimal has fractional part"
            n
          ) ;value-error
          (decimal->short value)
        ) ;if
      ) ;let
    ) ;define

    ;; decimal->byte: 将decimal转换为byte（带舍入）
    (define (decimal->byte n)
      (let ((int-val (decimal->int n)))
        (if (or (> int-val 127) (< int-val -128))
          (value-error "decimal->byte in (liii decimal type): value out of byte range" n)
          int-val
        ) ;if
      ) ;let
    ) ;define

    ;; decimal->byte-exact: 精确转换，如果丢失信息则报错
    (define (decimal->byte-exact n)
      (let ((value (ensure-decimal "decimal->byte-exact" n)))
        (if (not (= (decimal-scale value) 0))
          (value-error "decimal->byte-exact in (liii decimal type): decimal has fractional part"
            n
          ) ;value-error
          (decimal->byte value)
        ) ;if
      ) ;let
    ) ;define

    ;; decimal->double: 将decimal转换为double（可能丢失精度）
    (define (decimal->double n)
      (let ((value (ensure-decimal "decimal->double" n)))
        (string->number (decimal->string value))
      ) ;let
    ) ;define

    ;; decimal->float: 将decimal转换为float（可能丢失精度）
    (define (decimal->float n)
      (let ((value (ensure-decimal "decimal->float" n)))
        (exact->inexact (string->number (decimal->string value)))
      ) ;let
    ) ;define

    ;; decimal->rational: 将decimal转换为有理数（返回分子分母对）
    (define (decimal->rational n)
      (let* ((value (ensure-decimal "decimal->rational" n))
             (scale (decimal-scale value))
             (unscaled (decimal-unscaled value))
            ) ;
        (if (>= scale 0)
          (cons unscaled (bigint-expt (integer->bigint 10) scale))
          (cons (bigint-multiply unscaled (bigint-expt (integer->bigint 10) (- scale)))
            (integer->bigint 1)
          ) ;cons
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
