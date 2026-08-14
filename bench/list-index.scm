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
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See
;; the License for the specific language governing permissions and
;; limitations under the License.
;;

;; list-index 性能基准测试
;; 对比旧 Scheme 实现（srfi-1.scm 中的递归循环）与新的 C 实现
;; （src/s7_liii_list.c 的 g_list_index）
;;
;; 旧实现只遍历第一个列表（多列表形式不被接受），
;; 因此本基准只对比单列表形式。

(import (liii timeit) (liii list) (scheme base))

(define (bench name stmt number)
  (let ((elapsed (timeit stmt '() number)))
    (display name)
    (display ": ")
    (display elapsed)
    (display " 秒 (")
    (display number)
    (display " 次)\n")
  ) ;let
) ;define

;; 旧 Scheme 实现（迁移到 C 之前的版本）

(define (list-index-old pred l)
  (let loop
    ((index 0) (l l))
    (if (null? l) #f (if (pred (car l)) index (loop (+ index 1) (cdr l))))
  ) ;let
) ;define

(define (run-benchmarks)
  (display "=== list-index 性能测试（旧 Scheme 实现 vs 新 C 实现）===\n\n"
  ) ;display

  (define (compare name pred lst number)
    (display name)
    (display "\n")
    (bench "  旧 Scheme 实现" (lambda () (list-index-old pred lst)) number)
    (bench "  新 C 实现     " (lambda () (list-index pred lst)) number)
    (newline)
  ) ;define

  ;; 命中位置在列表头部
  (compare "头部命中（1K 元素）" even? (iota 1000) 1000)
  ;; 命中位置在列表尾部附近
  (compare "尾部命中（1K 元素，找第一个 >998）"
    (lambda (x) (> x 998))
    (iota 1000)
    1000
  ) ;compare
  ;; 未命中，需要遍历完整列表
  (compare "未命中（10K 元素）" (lambda (x) (> x 10000)) (iota 10000) 100)
  (compare "未命中（100K 元素）"
    (lambda (x) (> x 100000))
    (iota 100000)
    10
  ) ;compare
  ;; 谓词本身开销较大时
  (compare "中列表（复杂谓词）"
    (lambda (x) (and (> x 1000) (even? (length (list x)))))
    (iota 10000)
    100
  ) ;compare

  (display "多列表形式（仅新 C 实现支持，展示用途）\n")
  (bench "  新 C 实现（双列表 =）"
    (lambda () (list-index = (iota 10000) (map (lambda (x) (- x 1)) (iota 10000))))
    10
  ) ;bench
) ;define

(run-benchmarks)
