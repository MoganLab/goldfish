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

;; 评估 (import (liii json)) 的加载性能
;; 冷加载：每个库只测一次（受 defined? 缓存限制，单次进程内只能测一次）
;; 热导入：库已加载后再次 import 的开销（defined? 缓存命中 + varlet）

(import (scheme time))

(define (report title time-val)
  (display "[")
  (display title)
  (display "] time=")
  (display time-val)
  (display "s")
  (newline)
) ;define

(display "=== (import (liii json)) 加载性能基准测试 ===")
(newline)
(newline)

;; 冷加载分层测量：liii/base -> liii/list -> liii/json（含 guenchi/json）
(define t0 (current-second))
(import (liii base))
(report "冷加载 (liii base)" (- (current-second) t0))

(define t1 (current-second))
(import (liii list))
(report "冷加载 (liii list)" (- (current-second) t1))

(define t2 (current-second))
(import (liii json))
(report "冷加载 (liii json)" (- (current-second) t2))

;; 热导入：defined? 缓存命中后再次 import 的开销
(import (liii timeit))

(define warm-iter 1000)
(define t3
  (timeit (lambda () (import (liii json))) '() warm-iter))
(display "[热导入 (liii json)] iterations=")
(display warm-iter)
(display " total=")
(display t3)
(display "s avg=")
(display (/ t3 warm-iter))
(display "s")
(newline)

(newline)
(display "=== 测试完成 ===")
(newline)
