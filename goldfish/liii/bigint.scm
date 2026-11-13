;;; liii/bigint.scm - 任意精度整数实现主模块
;;;
;;; 聚合 (liii bigint) 所有子模块，提供完整的任意精度整数操作接口。
;;;
;;; Copyright (c) 2024-2026 Liii Network
;;; All Rights Reserved

(define-library (liii bigint)
  (import (scheme base)
    (liii bigint record)
    (liii bigint constant)
    (liii bigint predicate)
    (liii bigint compare)
    (liii bigint conversion)
    (liii bigint arithmetic)
    (liii bigint math)
    (liii bigint bitwise)
    (liii bigint prime)
  ) ;import

  (export bigint? string->bigint integer->bigint make-random-bigint
    bigint->string bigint-add bigint-subtract bigint-multiply bigint-divide
    bigint-remainder bigint-divide-and-remainder bigint-quotient bigint-modulo
    bigint-negate bigint-abs bigint-gcd bigint-expt bigint-sqrt
    bigint-sqrt-and-remainder bigint-rootn bigint-rootn-and-remainder
    bigint-signum bigint-zero? bigint-positive? bigint-negative? bigint=?
    bigint<? bigint>? bigint<=? bigint>=? bigint-and bigint-or bigint-xor
    bigint-not bigint-arithmetic-shift bigint-bit-set? bigint-bit-set
    bigint-bit-clear bigint-bit-flip bigint-bit-length bigint-bit-count
    bigint-lowest-bit-set bigint-even? bigint-odd? bigint-zero bigint-one
    bigint-two bigint-ten bigint-mod bigint-mod-expt bigint-mod-inverse
    bigint-prime? bigint-next-prime
  ) ;export

  (begin
  ) ;begin
) ;define-library
