;;; liii/decimal/math-context.scm - MathContext 支持（精度和舍入模式）
;;;
;;; 参考 JDK java.math.MathContext 设计
;;; 用于控制 decimal 运算的精度和舍入行为
;;;
;;; Copyright (c) 2026 Liii Network
;;; All Rights Reserved

(define-library (liii decimal math-context)
  (import (scheme base) (liii error))

  (export math-context? make-math-context math-context-precision
    math-context-rounding-mode math-context-decimal32 math-context-decimal64
    math-context-decimal128 math-context-unlimited
    math-context-valid-rounding-mode?
  ) ;export

  (begin
    ;; MathContext 记录类型：包含精度和舍入模式
    (define-record-type <math-context>
      (make-raw-math-context precision rounding-mode)
      math-context?
      (precision math-context-precision)
      (rounding-mode math-context-rounding-mode)
    ) ;define-record-type

    ;; 有效的舍入模式列表
    (define valid-rounding-modes
      '(up down ceiling floor half-up half-down half-even unnecessary)
    ) ;define

    ;; 验证舍入模式是否有效
    (define (math-context-valid-rounding-mode? mode)
      (memq mode valid-rounding-modes)
    ) ;define

    ;; 构造函数
    (define (make-math-context precision rounding-mode)
      (cond ((not (integer? precision))
             (type-error "make-math-context in (liii decimal math-context): precision must be an integer"
               precision
             ) ;type-error
            ) ;
            ((< precision 0)
             (value-error "make-math-context in (liii decimal math-context): precision must be non-negative"
               precision
             ) ;value-error
            ) ;
            ((not (math-context-valid-rounding-mode? rounding-mode))
             (value-error "make-math-context in (liii decimal math-context): invalid rounding mode"
               rounding-mode
             ) ;value-error
            ) ;
            (else (make-raw-math-context precision rounding-mode))
      ) ;cond
    ) ;define

    ;; 标准 MathContext 常量
    ;; DECIMAL32: 7位精度，HALF_EVEN 舍入
    (define math-context-decimal32 (make-raw-math-context 7 'half-even))

    ;; DECIMAL64: 16位精度，HALF_EVEN 舍入
    (define math-context-decimal64 (make-raw-math-context 16 'half-even))

    ;; DECIMAL128: 34位精度，HALF_EVEN 舍入
    (define math-context-decimal128 (make-raw-math-context 34 'half-even))

    ;; UNLIMITED: 无限精度（0表示不限制），UNNECESSARY舍入
    (define math-context-unlimited (make-raw-math-context 0 'unnecessary))
  ) ;begin
) ;define-library
