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

;; string-base64-decode 性能基准：仅测 string-base64-decode 一个函数

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

(define tiny-n 8)

(define small-n 100)

(define medium-n 1024)

(define large-n (* 10 1024))

(define ascii-tiny (build-ascii-string tiny-n))

(define ascii-small (build-ascii-string small-n))

(define ascii-medium (build-ascii-string medium-n))

(define ascii-large (build-ascii-string large-n))

(define enc-tiny (string-base64-encode ascii-tiny))

(define enc-small (string-base64-encode ascii-small))

(define enc-medium (string-base64-encode ascii-medium))

(define enc-large (string-base64-encode ascii-large))

(define tiny-iter 1500)

(define small-iter 150)

(define medium-iter 15)

(define large-iter 2)

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
  (string-base64-decode enc-small)
) ;do

(display "=== string-base64-decode 性能基准测试 ===")
(newline)
(newline)

(let ((t (timeit (lambda () (string-base64-decode enc-tiny)) '() tiny-iter)))
  (report "string-base64-decode tiny(8B)" tiny-iter t)
) ;let
(let ((t (timeit (lambda () (string-base64-decode enc-small)) '() small-iter)))
  (report "string-base64-decode small(100B)" small-iter t)
) ;let
(let ((t (timeit (lambda () (string-base64-decode enc-medium)) '() medium-iter)))
  (report "string-base64-decode medium(1KB)" medium-iter t)
) ;let
(let ((t (timeit (lambda () (string-base64-decode enc-large)) '() large-iter)))
  (report "string-base64-decode large(10KB)" large-iter t)
) ;let
