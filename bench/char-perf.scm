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

;; (scheme char) 表驱动函数调用性能基准测试

(import (liii base) (liii timeit) (scheme char))

(define (report title iterations time-val)
  (display "[")
  (display title)
  (display "] iterations=")
  (display iterations)
  (display " total=")
  (display time-val)
  (display "s avg=")
  (display (/ time-val iterations))
  (display "s")
  (newline)
) ;define

(define iter 100000)

(display "=== (scheme char) 函数调用性能基准测试 ===")
(newline)
(newline)

(let ((t (timeit (lambda () (char-upcase #\a)) '() iter)))
  (report "char-upcase ASCII(命中)" iter t)
) ;let

(let ((t (timeit (lambda () (char-upcase #\λ)) '() iter)))
  (report "char-upcase 希腊字母(命中)" iter t)
) ;let

(let ((t (timeit (lambda () (char-upcase #\中)) '() iter)))
  (report "char-upcase CJK(未命中)" iter t)
) ;let

(let ((t (timeit (lambda () (char-downcase #\A)) '() iter)))
  (report "char-downcase ASCII(命中)" iter t)
) ;let

(let ((t (timeit (lambda () (char-alphabetic? #\a)) '() iter)))
  (report "char-alphabetic?(是)" iter t)
) ;let

(let ((t (timeit (lambda () (char-alphabetic? #\x10FFFF)) '() iter)))
  (report "char-alphabetic?(>205743)" iter t)
) ;let

(let ((t (timeit (lambda () (char-upper-case? #\A)) '() iter)))
  (report "char-upper-case?(是)" iter t)
) ;let

(let ((t (timeit (lambda () (char-lower-case? #\a)) '() iter)))
  (report "char-lower-case?(是)" iter t)
) ;let

(newline)
(display "=== 测试完成 ===")
(newline)
