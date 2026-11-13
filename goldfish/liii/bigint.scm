;;; liii/bigint.scm - 任意精度整数实现主模块
;;;
;;; 聚合 (liii bigint) 所有子模块，提供完整的任意精度整数操作接口。
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
