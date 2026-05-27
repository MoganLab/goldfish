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

(import (liii base)
  (liii json)
  (liii timeit)
) ;import

(define (build-json-string n)
  (let ((out (open-output-string)))
    (display "{" out)
    (do ((i 0 (+ i 1)))
      ((= i n))
      (when (> i 0)
        (display "," out)
      ) ;when
      (display "\"k" out)
      (display i out)
      (display "\":" out)
      (display i out)
    ) ;do
    (display "}" out)
    (get-output-string out)
  ) ;let
) ;define

(define (build-array-string n)
  (let ((out (open-output-string)))
    (display "[" out)
    (do ((i 0 (+ i 1)))
      ((= i n))
      (when (> i 0)
        (display "," out)
      ) ;when
      (display i out)
    ) ;do
    (display "]" out)
    (get-output-string out)
  ) ;let
) ;define

(define bench-key-count 200)
(define bench-array-length 200)

(define bench-obj-json (build-json-string bench-key-count))
(define bench-arr-json (build-array-string bench-array-length))
(define bench-obj-scm (string->json bench-obj-json))
(define bench-arr-scm (string->json bench-arr-json))
(define bench-ref-key (string-append "k" (number->string (quotient bench-key-count 2))))

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
  ((= i 20))
  (string->json bench-obj-json)
  (json->string bench-obj-scm)
  (json-ref bench-obj-scm bench-ref-key)
  (json-contains-key? bench-obj-scm bench-ref-key)
  (json-keys bench-obj-scm)
  (json-object? bench-obj-scm)
) ;do

(define parse-iter 50)
(define stringify-iter 50)
(define ref-iter 2000)
(define contains-iter 2000)
(define keys-iter 500)
(define typecheck-iter 5000)

(display "=== (liii json) 性能基准测试 ===")
(newline)
(newline)

(let ((t (timeit (lambda () (string->json bench-obj-json)) '() parse-iter)))
  (report "解析对象(string->json)" parse-iter t)
) ;let

(let ((t (timeit (lambda () (string->json bench-arr-json)) '() parse-iter)))
  (report "解析数组(string->json)" parse-iter t)
) ;let

(let ((t (timeit (lambda () (json->string bench-obj-scm)) '() stringify-iter)))
  (report "序列化对象(json->string)" stringify-iter t)
) ;let

(let ((t (timeit (lambda () (json->string bench-arr-scm)) '() stringify-iter)))
  (report "序列化数组(json->string)" stringify-iter t)
) ;let

(let ((t (timeit (lambda () (json-ref bench-obj-scm bench-ref-key)) '() ref-iter)))
  (report "对象读取(json-ref)" ref-iter t)
) ;let

(let ((t (timeit (lambda () (json-ref bench-arr-scm 50)) '() ref-iter)))
  (report "数组读取(json-ref)" ref-iter t)
) ;let

(let ((t (timeit (lambda () (json-contains-key? bench-obj-scm bench-ref-key)) '() contains-iter)))
  (report "键存在(json-contains-key?)" contains-iter t)
) ;let

(let ((t (timeit (lambda () (json-keys bench-obj-scm)) '() keys-iter)))
  (report "获取键(json-keys)" keys-iter t)
) ;let

(let ((t (timeit (lambda () (json-object? bench-obj-scm)) '() typecheck-iter)))
  (report "类型检查(json-object?)" typecheck-iter t)
) ;let

(let ((t (timeit (lambda () (json-array? bench-arr-scm)) '() typecheck-iter)))
  (report "类型检查(json-array?)" typecheck-iter t)
) ;let

(newline)
(display "=== 测试完成 ===")
(newline)
