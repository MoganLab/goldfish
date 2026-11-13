;;; liii/bigint/constant.scm - bigint 常量
;;;
;;; 提供常用 bigint 常量，避免高层库之间因为常量定义产生依赖方向问题。
;;;
;;; Copyright (c) 2024 Liii Network
;;; All Rights Reserved

(define-library (liii bigint constant)
  (import (scheme base) (liii bigint record))

  (export bigint-zero bigint-one bigint-two bigint-ten)

  (begin
    (define bigint-zero (make-bigint 0 '()))
    (define bigint-one (make-bigint 1 '(1)))
    (define bigint-two (make-bigint 1 '(2)))
    (define bigint-ten (make-bigint 1 '(10)))
  ) ;begin
) ;define-library
