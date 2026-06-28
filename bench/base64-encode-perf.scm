;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

;; base64-encode 性能基准：仅测统一入口 base64-encode（按参数类型分发到 string / bytevector）

(import (liii base) (liii base64) (liii timeit))

(define (build-ascii-string n)
  (let ((out (open-output-string)))
    (do ((i 0 (+ i 1)))
      ((= i n))
      (display (integer->char (+ 97 (modulo i 26))) out)
    ) ;do
    (get-output-string out)
  ) ;let
) ;define

(define (build-binary-bv n)
  (let ((bv (make-bytevector n)))
    (do ((i 0 (+ i 1)))
      ((= i n))
      (bytevector-u8-set! bv i (modulo (* i 31) 256))
    ) ;do
    bv
  ) ;let
) ;define

(define medium-n 1024)

(define ascii-medium (build-ascii-string medium-n))

(define bv-medium (build-binary-bv medium-n))

(define iter 30)

(define (report title iterations time-val)
  (display "[")
  (display title)
  (display "] iterations=")
  (display iterations)
  (display " time=")
  (display time-val)
  (display "s")
  (newline)
) ;define

(do ((i 0 (+ i 1)))
  ((= i 5))
  (base64-encode ascii-medium)
  (base64-encode bv-medium)
) ;do

(display "=== base64-encode 性能基准测试 ===")
(newline)
(newline)

(let ((t (timeit (lambda () (base64-encode ascii-medium)) '() iter)))
  (report "base64-encode string(1KB)" iter t)
) ;let
(let ((t (timeit (lambda () (base64-encode bv-medium)) '() iter)))
  (report "base64-encode bytevector(1KB)" iter t)
) ;let
