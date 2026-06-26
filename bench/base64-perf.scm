;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

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

(define tiny-n 8)

(define small-n 100)

(define medium-n 1024)

(define large-n (* 10 1024))

(define ascii-tiny (build-ascii-string tiny-n))

(define ascii-small (build-ascii-string small-n))

(define ascii-medium (build-ascii-string medium-n))

(define ascii-large (build-ascii-string large-n))

(define bv-tiny (build-binary-bv tiny-n))

(define bv-small (build-binary-bv small-n))

(define bv-medium (build-binary-bv medium-n))

(define bv-large (build-binary-bv large-n))

(define enc-ascii-tiny (string-base64-encode ascii-tiny))

(define enc-ascii-small (string-base64-encode ascii-small))

(define enc-ascii-medium (string-base64-encode ascii-medium))

(define enc-ascii-large (string-base64-encode ascii-large))

(define enc-bv-tiny (bytevector-base64-encode bv-tiny))

(define enc-bv-small (bytevector-base64-encode bv-small))

(define enc-bv-medium (bytevector-base64-encode bv-medium))

(define enc-bv-large (bytevector-base64-encode bv-large))

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
  (string-base64-decode enc-ascii-small)
  (bytevector-base64-encode bv-small)
  (bytevector-base64-decode enc-bv-small)
) ;do

(define str-enc-tiny-iter 3000)

(define str-enc-small-iter 300)

(define str-enc-medium-iter 30)

(define str-enc-large-iter 3)

(define str-decode-tiny-iter 1500)

(define str-decode-small-iter 150)

(define str-decode-medium-iter 15)

(define str-decode-large-iter 2)

(define bv-enc-tiny-iter 3000)

(define bv-enc-small-iter 300)

(define bv-enc-medium-iter 30)

(define bv-enc-large-iter 3)

(define bv-decode-tiny-iter 1500)

(define bv-decode-small-iter 150)

(define bv-decode-medium-iter 15)

(define bv-decode-large-iter 2)

(define unified-enc-iter 30)

(define unified-decode-iter 15)

(display "=== (liii base64) 性能基准测试 ===")
(newline)
(newline)

(display "--- string-base64-encode ---")
(newline)
(let ((t (timeit (lambda () (string-base64-encode ascii-tiny)) '() str-enc-tiny-iter))
     ) ;
  (report "string-base64-encode tiny(8B)" str-enc-tiny-iter t)
) ;let
(let ((t (timeit (lambda () (string-base64-encode ascii-small)) '() str-enc-small-iter)
      ) ;t
     ) ;
  (report "string-base64-encode small(100B)" str-enc-small-iter t)
) ;let
(let ((t (timeit (lambda () (string-base64-encode ascii-medium)) '() str-enc-medium-iter)
      ) ;t
     ) ;
  (report "string-base64-encode medium(1KB)" str-enc-medium-iter t)
) ;let
(let ((t (timeit (lambda () (string-base64-encode ascii-large)) '() str-enc-large-iter)
      ) ;t
     ) ;
  (report "string-base64-encode large(10KB)" str-enc-large-iter t)
) ;let
(newline)

(display "--- string-base64-decode ---")
(newline)
(let ((t (timeit (lambda () (string-base64-decode enc-ascii-tiny))
           '()
           str-decode-tiny-iter
         ) ;timeit
      ) ;t
     ) ;
  (report "string-base64-decode tiny(8B)" str-decode-tiny-iter t)
) ;let
(let ((t (timeit (lambda () (string-base64-decode enc-ascii-small))
           '()
           str-decode-small-iter
         ) ;timeit
      ) ;t
     ) ;
  (report "string-base64-decode small(100B)" str-decode-small-iter t)
) ;let
(let ((t (timeit (lambda () (string-base64-decode enc-ascii-medium))
           '()
           str-decode-medium-iter
         ) ;timeit
      ) ;t
     ) ;
  (report "string-base64-decode medium(1KB)" str-decode-medium-iter t)
) ;let
(let ((t (timeit (lambda () (string-base64-decode enc-ascii-large))
           '()
           str-decode-large-iter
         ) ;timeit
      ) ;t
     ) ;
  (report "string-base64-decode large(10KB)" str-decode-large-iter t)
) ;let
(newline)

(display "--- bytevector-base64-encode ---")
(newline)
(let ((t (timeit (lambda () (bytevector-base64-encode bv-tiny)) '() bv-enc-tiny-iter))
     ) ;
  (report "bytevector-base64-encode tiny(8B)" bv-enc-tiny-iter t)
) ;let
(let ((t (timeit (lambda () (bytevector-base64-encode bv-small)) '() bv-enc-small-iter)
      ) ;t
     ) ;
  (report "bytevector-base64-encode small(100B)" bv-enc-small-iter t)
) ;let
(let ((t (timeit (lambda () (bytevector-base64-encode bv-medium)) '() bv-enc-medium-iter)
      ) ;t
     ) ;
  (report "bytevector-base64-encode medium(1KB)" bv-enc-medium-iter t)
) ;let
(let ((t (timeit (lambda () (bytevector-base64-encode bv-large)) '() bv-enc-large-iter)
      ) ;t
     ) ;
  (report "bytevector-base64-encode large(10KB)" bv-enc-large-iter t)
) ;let
(newline)

(display "--- bytevector-base64-decode ---")
(newline)
(let ((t (timeit (lambda () (bytevector-base64-decode enc-bv-tiny))
           '()
           bv-decode-tiny-iter
         ) ;timeit
      ) ;t
     ) ;
  (report "bytevector-base64-decode tiny(8B)" bv-decode-tiny-iter t)
) ;let
(let ((t (timeit (lambda () (bytevector-base64-decode enc-bv-small))
           '()
           bv-decode-small-iter
         ) ;timeit
      ) ;t
     ) ;
  (report "bytevector-base64-decode small(100B)" bv-decode-small-iter t)
) ;let
(let ((t (timeit (lambda () (bytevector-base64-decode enc-bv-medium))
           '()
           bv-decode-medium-iter
         ) ;timeit
      ) ;t
     ) ;
  (report "bytevector-base64-decode medium(1KB)" bv-decode-medium-iter t)
) ;let
(let ((t (timeit (lambda () (bytevector-base64-decode enc-bv-large))
           '()
           bv-decode-large-iter
         ) ;timeit
      ) ;t
     ) ;
  (report "bytevector-base64-decode large(10KB)" bv-decode-large-iter t)
) ;let
(newline)

(display "--- base64-encode / base64-decode 统一入口 ---")
(newline)
(let ((t (timeit (lambda () (base64-encode ascii-medium)) '() unified-enc-iter)))
  (report "base64-encode string(1KB)" unified-enc-iter t)
) ;let
(let ((t (timeit (lambda () (base64-encode bv-medium)) '() unified-enc-iter)))
  (report "base64-encode bytevector(1KB)" unified-enc-iter t)
) ;let
(let ((t (timeit (lambda () (base64-decode enc-ascii-medium)) '() unified-decode-iter)
      ) ;t
     ) ;
  (report "base64-decode string(1KB)" unified-decode-iter t)
) ;let
(let ((t (timeit (lambda () (base64-decode enc-bv-medium)) '() unified-decode-iter)))
  (report "base64-decode bytevector(1KB)" unified-decode-iter t)
) ;let
