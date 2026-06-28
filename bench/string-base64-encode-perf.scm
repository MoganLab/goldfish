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

;; string-base64-encode 性能基准：仅测 string-base64-encode 一个函数

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
  (string-base64-encode ascii-small)
) ;do

(display "=== string-base64-encode 性能基准测试 ===")
(newline)
(newline)

(let ((t (timeit (lambda () (string-base64-encode ascii-tiny)) '() tiny-iter)))
  (report "string-base64-encode tiny(8B)" tiny-iter t)
) ;let
(let ((t (timeit (lambda () (string-base64-encode ascii-small)) '() small-iter)))
  (report "string-base64-encode small(100B)" small-iter t)
) ;let
(let ((t (timeit (lambda () (string-base64-encode ascii-medium)) '() medium-iter)))
  (report "string-base64-encode medium(1KB)" medium-iter t)
) ;let
(let ((t (timeit (lambda () (string-base64-encode ascii-large)) '() large-iter)))
  (report "string-base64-encode large(10KB)" large-iter t)
) ;let
