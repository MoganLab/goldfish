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

;; json->string 性能对比基准：C++ 实现 (g_json->string) vs 历史 Scheme 实现
;; 运行方式: bin/gf bench/json-to-string-perf.scm

(import (liii base)
  (liii json)
  (liii timeit)
) ;import

;; 历史 Scheme 实现（原 (guenchi json) 中的 json->string），原样保留用于对比
(define (json->string/scheme json-scm)
  (when (procedure? json-scm)
    (type-error "json->string: input must not be a procedure")
  ) ;when
  (let ((out (open-output-string)))
    (define (write-scalar x)
      (cond ((string? x) (display (json-string-escape x) out))
            ((number? x) (display (number->string x) out))
            ((boolean? x) (display (if x "true" "false") out))
            ((symbol? x) (display (symbol->string x) out))
            ((null? x) (display "{}" out))
            (else (type-error "Unexpected x: " x))
      ) ;cond
    ) ;define
    (define (write-json x)
      (cond ((vector? x)
             (display "[" out)
             (let ((len (vector-length x)))
               (do ((i 0 (+ i 1)))
                 ((= i len))
                 (when (> i 0)
                   (display "," out)
                 ) ;when
                 (let ((k (vector-ref x i)))
                   (cond ((vector? k) (write-json k))
                         ((pair? k) (write-json k))
                         (else (write-scalar k))
                   ) ;cond
                 ) ;let
               ) ;do
             ) ;let
             (display "]" out)
            ) ;
            ((pair? x)
             (display "{" out)
             (let loop
               ((lst x) (i 0))
               (unless (null? lst)
                 (let ((d (car lst)))
                   (when (> i 0)
                     (display "," out)
                   ) ;when
                   (if (null? d)
                     (display "{}" out)
                     (begin
                       (let ((len (length d)))
                         (when (not (or (= len 0) (= len -1) (>= len 2)))
                           (value-error d " must be null, pair, or list with at least 2 elements")
                         ) ;when
                       ) ;let
                       (let ((k (loose-car d)) (v (loose-cdr d)))
                         (write-scalar k)
                         (display ":" out)
                         (cond ((null? v) (display "{}" out))
                               ((list? v) (write-json v))
                               ((vector? v) (write-json v))
                               (else (write-scalar v))
                         ) ;cond
                       ) ;let
                     ) ;begin
                   ) ;if
                   (loop (cdr lst) (+ i 1))
                 ) ;let
               ) ;unless
             ) ;let
             (display "}" out)
            ) ;
            (else (write-scalar x))
      ) ;cond
    ) ;define
    (write-json json-scm)
    (get-output-string out)
  ) ;let
) ;define

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

(define bench-obj-scm (string->json (build-json-string 200)))
(define bench-arr-scm (string->json (build-array-string 200)))
(define bench-nested-scm
  '((user (id . 1001)
      (name . "Alice")
      (active . #t)
      (email . null)
      (tags . #("dev" "scheme" "json"))
      (profile (age . 21) (height . 168.5) (hobbies . #("music" "reading"))))
    (scores . #(98 87 93)))
) ;define

;; 正确性 sanity check：两种实现输出必须一致
(unless (equal? (json->string bench-obj-scm) (json->string/scheme bench-obj-scm))
  (error 'value-error "object output mismatch")
) ;unless
(unless (equal? (json->string bench-arr-scm) (json->string/scheme bench-arr-scm))
  (error 'value-error "array output mismatch")
) ;unless
(unless (equal? (json->string bench-nested-scm) (json->string/scheme bench-nested-scm))
  (error 'value-error "nested output mismatch")
) ;unless

(define iter 200)

(define (report title time-val)
  (display "[")
  (display title)
  (display "] iterations=")
  (display iter)
  (display " time=")
  (display time-val)
  (display "s")
  (newline)
) ;define

;; warmup
(do ((i 0 (+ i 1)))
  ((= i 20))
  (json->string bench-obj-scm)
  (json->string/scheme bench-obj-scm)
) ;do

(display "=== json->string C++ vs Scheme 性能对比 ===")
(newline)
(newline)

(let ((t (timeit (lambda () (json->string bench-obj-scm)) '() iter)))
  (report "对象/C++" t)
) ;let
(let ((t (timeit (lambda () (json->string/scheme bench-obj-scm)) '() iter)))
  (report "对象/Scheme" t)
) ;let
(let ((t (timeit (lambda () (json->string bench-arr-scm)) '() iter)))
  (report "数组/C++" t)
) ;let
(let ((t (timeit (lambda () (json->string/scheme bench-arr-scm)) '() iter)))
  (report "数组/Scheme" t)
) ;let
(let ((t (timeit (lambda () (json->string bench-nested-scm)) '() (* 10 iter))))
  (display "[嵌套/C++] iterations=")
  (display (* 10 iter))
  (display " time=")
  (display t)
  (display "s")
  (newline)
) ;let
(let ((t (timeit (lambda () (json->string/scheme bench-nested-scm)) '() (* 10 iter))))
  (display "[嵌套/Scheme] iterations=")
  (display (* 10 iter))
  (display " time=")
  (display t)
  (display "s")
  (newline)
) ;let

(newline)
(display "=== 测试完成 ===")
(newline)
