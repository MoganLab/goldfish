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

;; append 性能基准测试
;; 对比 s7_append GC 保护修复（temp7 保护第二个参数）前后的性能

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

(define (run-benchmarks)
  (display "=== append 性能测试 ===\n\n")

  ;; 短列表（两参数优化路径 s7_append）
  (bench "短列表 (append '(1 2 3) '(4 5 6))"
    (lambda () (append '(1 2 3) '(4 5 6)))
    1000000
  ) ;bench

  ;; 中等列表（两参数优化路径 s7_append）
  (let ((a (iota 100)) (b (iota 100)))
    (bench "中列表 (append (iota 100) (iota 100))"
      (lambda () (append a b))
      10000
    ) ;bench
  ) ;let

  ;; 长列表（两参数优化路径 s7_append，复制循环中触发 GC）
  (let ((a (iota 1000)) (b (iota 1000)))
    (bench "长列表 (append (iota 1000) (iota 1000))"
      (lambda () (append a b))
      1000
    ) ;bench
  ) ;let

  ;; 多列表（g_append -> g_list_append 路径）
  (let ((a (iota 10)) (b (iota 10)) (c (iota 10)))
    (bench "多列表 (append a b c)，各 10 元素"
      (lambda () (append a b c))
      100000
    ) ;bench
  ) ;let

  ;; 循环构建列表（gf doc 模糊匹配的实际使用场景）
  (bench "循环构建 300 元素列表"
    (lambda ()
      (let loop
        ((i 0) (acc '()))
        (if (= i 300) acc (loop (+ i 1) (append acc (list (number->string i)))))
      ) ;let
    ) ;lambda
    100
  ) ;bench
) ;define

(run-benchmarks)
