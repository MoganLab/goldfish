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

;; count 性能基准测试
;; 对比旧 Scheme 实现（srfi-1.scm 中的递归循环）与新的 C 实现
;; （src/s7_liii_list.c 的 g_count）
;;
;; 旧实现只遍历第一个列表（多列表形式被忽略），
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

(define (count-old pred list1)
  (let lp
    ((lis list1) (i 0))
    (if (null-list? lis) i (lp (cdr lis) (if (pred (car lis)) (+ i 1) i)))
  ) ;let
) ;define

(define (run-benchmarks)
  (display "=== count 性能测试（旧 Scheme 实现 vs 新 C 实现）===\n\n")

  (define (compare name pred lst number)
    (display name)
    (display "\n")
    (bench "  旧 Scheme 实现" (lambda () (count-old pred lst)) number)
    (bench "  新 C 实现     " (lambda () (count pred lst)) number)
    (newline)
  ) ;define

  (compare "空列表" even? '() 100000)
  (compare "小列表（1K 元素，偶数计数）" even? (iota 1000) 1000)
  (compare "中列表（10K 元素，偶数计数）" even? (iota 10000) 100)
  (compare "大列表（100K 元素，偶数计数）" even? (iota 100000) 10)
  (compare "中列表（复杂谓词）"
    (lambda (x) (and (> x 1000) (even? (length (list x)))))
    (iota 10000)
    100
  ) ;compare

  (display "多列表形式（仅新 C 实现支持，展示用途）\n")
  (bench "  新 C 实现（双列表 =）"
    (lambda () (count = (iota 10000) (map (lambda (x) (- x 1)) (iota 10000))))
    10
  ) ;bench
) ;define

(run-benchmarks)
