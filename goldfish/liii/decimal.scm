;;; liii/decimal.scm - 十进制定点数主接口
;;;
;;; 聚合 (liii decimal) 所有子模块，提供完整的十进制定点数操作接口。
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

(define-library (liii decimal)
  (import (scheme base)
    (liii bigint)
    (liii decimal record)
    (liii decimal math-context)
    (liii decimal compare)
    (liii decimal conversion)
    (liii decimal rounding)
    (liii decimal arithmetic)
    (liii decimal cast)
    (liii decimal advanced)
  ) ;import

  (export decimal? make-decimal decimal-unscaled-value decimal-scale
    decimal-precision decimal-signum decimal-ulp string->decimal decimal->string
    decimal->plain-string decimal->engineering-string decimal->scientific-string
    decimal->long decimal->long-exact decimal->int decimal->int-exact
    decimal->short decimal->short-exact decimal->byte decimal->byte-exact
    decimal->double decimal->float decimal->rational decimal-add
    decimal-subtract decimal-multiply decimal-divide decimal-set-scale
    decimal-scale-by-power-of-ten decimal-move-point-left
    decimal-move-point-right decimal-divide-to-integral-value decimal-remainder
    decimal-divide-and-remainder decimal-strip-trailing-zeros decimal-plus
    decimal-exact-integer? decimal-negate decimal-abs decimal-round-up
    decimal-round-down decimal-round-ceiling decimal-round-floor
    decimal-round-half-up decimal-round-half-down decimal-round-half-even
    decimal-round-unnecessary decimal-floor decimal-ceiling decimal-truncate
    decimal=? decimal<? decimal>? decimal<=? decimal>=? decimal-max decimal-min
    decimal-equals? decimal-hash-code decimal-round decimal-to-bigint
    decimal-to-bigint-exact bigint->decimal integer->decimal long->decimal
    double->decimal rational->decimal decimal-pow decimal-sqrt decimal-zero?
    decimal-positive? decimal-negative? decimal-add-with-context
    decimal-subtract-with-context decimal-multiply-with-context
    decimal-divide-with-context decimal-pow-with-context
    decimal-sqrt-with-context decimal-abs-with-context
    decimal-negate-with-context decimal-plus-with-context make-math-context
    math-context? math-context-precision math-context-rounding-mode
    math-context-decimal32 math-context-decimal64 math-context-decimal128
    math-context-unlimited
  ) ;export

  (begin
  ) ;begin
) ;define-library
