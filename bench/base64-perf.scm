;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(import (scheme base) (liii base64) (liii timeit))

(define (build-string n)
  (let ((out (make-string n)))
    (do ((i 0 (+ i 1)))
      ((= i n) out)
      (string-set! out i (integer->char (+ 33 (modulo i 94))))
    ) ;do
  ) ;let
) ;define

(define (build-bytevector n)
  (let ((out (make-bytevector n)))
    (do ((i 0 (+ i 1)))
      ((= i n) out)
      (bytevector-u8-set! out i (modulo (* i 7) 256))
    ) ;do
  ) ;let
) ;define

(define tiny-str "Goldfish")

(define tiny-bv (string->utf8 tiny-str))

(define small-str (build-string 100))

(define small-bv (string->utf8 small-str))

(define medium-str (build-string 1024))

(define medium-bv (string->utf8 medium-str))

(define large-str (build-string 10240))

(define large-bv (string->utf8 large-str))

(define binary-bv (build-bytevector 1024))

(define tiny-encoded (string-base64-encode tiny-str))

(define small-encoded (string-base64-encode small-str))

(define medium-encoded (string-base64-encode medium-str))

(define large-encoded (string-base64-encode large-str))

(define binary-encoded (bytevector-base64-encode binary-bv))

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

;; warmup
(do ((i 0 (+ i 1)))
  ((= i 100))
  (base64-encode tiny-str)
  (base64-decode tiny-encoded)
  (bytevector-base64-encode tiny-bv)
  (bytevector-base64-decode (string->utf8 tiny-encoded))
) ;do

(display "=== (liii base64) 性能基准测试 ===")
(newline)
(newline)

(display "--- 字符串编码 ---")
(newline)

(let ((t (timeit (lambda () (string-base64-encode tiny-str)) '() 100000)))
  (report "string-base64-encode tiny (8B)" 100000 t)
) ;let

(let ((t (timeit (lambda () (string-base64-encode small-str)) '() 50000)))
  (report "string-base64-encode small (100B)" 50000 t)
) ;let

(let ((t (timeit (lambda () (string-base64-encode medium-str)) '() 10000)))
  (report "string-base64-encode medium (1KB)" 10000 t)
) ;let

(let ((t (timeit (lambda () (string-base64-encode large-str)) '() 1000)))
  (report "string-base64-encode large (10KB)" 1000 t)
) ;let

(newline)
(display "--- 字符串解码 ---")
(newline)

(let ((t (timeit (lambda () (string-base64-decode tiny-encoded)) '() 100000)))
  (report "string-base64-decode tiny (8B)" 100000 t)
) ;let

(let ((t (timeit (lambda () (string-base64-decode small-encoded)) '() 50000)))
  (report "string-base64-decode small (100B)" 50000 t)
) ;let

(let ((t (timeit (lambda () (string-base64-decode medium-encoded)) '() 10000)))
  (report "string-base64-decode medium (1KB)" 10000 t)
) ;let

(let ((t (timeit (lambda () (string-base64-decode large-encoded)) '() 1000)))
  (report "string-base64-decode large (10KB)" 1000 t)
) ;let

(newline)
(display "--- 字节向量编码 ---")
(newline)

(let ((t (timeit (lambda () (bytevector-base64-encode tiny-bv)) '() 100000)))
  (report "bytevector-base64-encode tiny (8B)" 100000 t)
) ;let

(let ((t (timeit (lambda () (bytevector-base64-encode small-bv)) '() 50000)))
  (report "bytevector-base64-encode small (100B)" 50000 t)
) ;let

(let ((t (timeit (lambda () (bytevector-base64-encode medium-bv)) '() 10000)))
  (report "bytevector-base64-encode medium (1KB)" 10000 t)
) ;let

(let ((t (timeit (lambda () (bytevector-base64-encode large-bv)) '() 1000)))
  (report "bytevector-base64-encode large (10KB)" 1000 t)
) ;let

(let ((t (timeit (lambda () (bytevector-base64-encode binary-bv)) '() 10000)))
  (report "bytevector-base64-encode binary (1KB)" 10000 t)
) ;let

(newline)
(display "--- 字节向量解码 ---")
(newline)

(let ((t (timeit (lambda () (bytevector-base64-decode (string->utf8 tiny-encoded)))
           '()
           100000
         ) ;timeit
      ) ;t
     ) ;
  (report "bytevector-base64-decode tiny (8B)" 100000 t)
) ;let

(let ((t (timeit (lambda () (bytevector-base64-decode (string->utf8 small-encoded)))
           '()
           50000
         ) ;timeit
      ) ;t
     ) ;
  (report "bytevector-base64-decode small (100B)" 50000 t)
) ;let

(let ((t (timeit (lambda () (bytevector-base64-decode (string->utf8 medium-encoded)))
           '()
           10000
         ) ;timeit
      ) ;t
     ) ;
  (report "bytevector-base64-decode medium (1KB)" 10000 t)
) ;let

(let ((t (timeit (lambda () (bytevector-base64-decode (string->utf8 large-encoded)))
           '()
           1000
         ) ;timeit
      ) ;t
     ) ;
  (report "bytevector-base64-decode large (10KB)" 1000 t)
) ;let

(let ((t (timeit (lambda () (bytevector-base64-decode binary-encoded)) '() 10000)))
  (report "bytevector-base64-decode binary (1KB)" 10000 t)
) ;let

(newline)
(display "--- 统一入口 ---")
(newline)

(let ((t (timeit (lambda () (base64-encode medium-str)) '() 10000)))
  (report "base64-encode string (1KB)" 10000 t)
) ;let

(let ((t (timeit (lambda () (base64-encode medium-bv)) '() 10000)))
  (report "base64-encode bytevector (1KB)" 10000 t)
) ;let

(let ((t (timeit (lambda () (base64-decode medium-encoded)) '() 10000)))
  (report "base64-decode string (1KB)" 10000 t)
) ;let

(let ((t (timeit (lambda () (base64-decode (string->utf8 medium-encoded))) '() 10000))
     ) ;
  (report "base64-decode bytevector (1KB)" 10000 t)
) ;let

(newline)
(display "=== 测试完成 ===")
(newline)
