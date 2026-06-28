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

;; bytevector-base64-encode 性能基准：仅测 bytevector-base64-encode 一个函数

(import (liii base) (liii base64) (liii timeit))

(define (build-binary-bv n)
  (let ((bv (make-bytevector n)))
    (do ((i 0 (+ i 1)))
      ((= i n))
      (bytevector-u8-set! bv i (modulo (* i 31) 256))
    ) ;do
    bv
  ) ;let
) ;define

(define tiny-n 8)

(define small-n 100)

(define medium-n 1024)

(define large-n (* 10 1024))

(define bv-tiny (build-binary-bv tiny-n))

(define bv-small (build-binary-bv small-n))

(define bv-medium (build-binary-bv medium-n))

(define bv-large (build-binary-bv large-n))

(define tiny-iter 3000)

(define small-iter 300)

(define medium-iter 30)

(define large-iter 3)

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
  (bytevector-base64-encode bv-small)
) ;do

(display "=== bytevector-base64-encode 性能基准测试 ===")
(newline)
(newline)

(let ((t (timeit (lambda () (bytevector-base64-encode bv-tiny)) '() tiny-iter)))
  (report "bytevector-base64-encode tiny(8B)" tiny-iter t)
) ;let
(let ((t (timeit (lambda () (bytevector-base64-encode bv-small)) '() small-iter)))
  (report "bytevector-base64-encode small(100B)" small-iter t)
) ;let
(let ((t (timeit (lambda () (bytevector-base64-encode bv-medium)) '() medium-iter)))
  (report "bytevector-base64-encode medium(1KB)" medium-iter t)
) ;let
(let ((t (timeit (lambda () (bytevector-base64-encode bv-large)) '() large-iter)))
  (report "bytevector-base64-encode large(10KB)" large-iter t)
) ;let
