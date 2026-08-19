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
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(import (liii base) (liii timeit))

(define bench-file "bench/read-perf-data.scm")

;; 生成一个较大的测试文件：若干个 define 与列表/字符串/数字混合的 form

(define (build-bench-file n)
  (let ((out (open-output-string)))
    (do ((i 0 (+ i 1)))
      ((= i n))
      (display "(define (bench-func-" out)
      (display i out)
      (display " x) ;; 函数注释 " out)
      (display i out)
      (newline out)
      (display "  (let ((y (* x " out)
      (display i out)
      (display ")))" out)
      (newline out)
      (display "    (list y \"中文字符串测试\" #\\A 'sym #(1 2 3) 3.14)))" out)
      (newline out)
      (newline out)
    ) ;do
    (with-output-to-file bench-file (lambda () (display (get-output-string out))))
  ) ;let
) ;define

(define form-count 500)
(build-bench-file form-count)

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

(define (read-all-from-file file)
  (let ((p (open-input-file file)))
    (let loop
      ()
      (let ((x (read p)))
        (if (eof-object? x) (begin (close-input-port p) 'done) (loop))
      ) ;let
    ) ;let
  ) ;let
) ;define

;; warmup
(do ((i 0 (+ i 1)))
  ((= i 3))
  (read-all-from-file bench-file)
) ;do

(define read-iter 30)

(define load-iter 30)

(display "=== read/load 性能基准测试 ===")
(newline)
(display "文件: ")
(display bench-file)
(display " (")
(display form-count)
(display " 个顶层 form)")
(newline)
(newline)

(let ((t (timeit (lambda () (read-all-from-file bench-file)) '() read-iter)))
  (report "读取文件(open-input-file+read)" read-iter t)
) ;let

(let ((t (timeit (lambda () (load bench-file)) '() load-iter)))
  (report "加载文件(load)" load-iter t)
) ;let

(newline)
(display "=== 测试完成 ===")
(newline)
