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

;; 评估 (import (liii list)) 的加载性能，按依赖顺序逐层冷加载归因
;; (liii list) 依赖: (liii error) (liii base) (srfi srfi-1)
;;                    (srfi srfi-13) -> (scheme char) (srfi srfi-175)

(import (scheme time))

(define (report title time-val)
  (display "[")
  (display title)
  (display "] time=")
  (display time-val)
  (display "s")
  (newline)
) ;define

(display "=== (import (liii list)) 加载性能分层归因 ===")
(newline)
(newline)

(define t0 (current-second))
(import (liii error))
(report "冷加载 (liii error)" (- (current-second) t0))

(define t1 (current-second))
(import (liii base))
(report "冷加载 (liii base)" (- (current-second) t1))

(define t2 (current-second))
(import (srfi srfi-1))
(report "冷加载 (srfi srfi-1)" (- (current-second) t2))

(define t3 (current-second))
(import (srfi srfi-175))
(report "冷加载 (srfi srfi-175)" (- (current-second) t3))

(define t4 (current-second))
(import (scheme char))
(report "冷加载 (scheme char)" (- (current-second) t4))

(define t5 (current-second))
(import (srfi srfi-13))
(report "冷加载 (srfi srfi-13)" (- (current-second) t5))

(define t6 (current-second))
(import (liii list))
(report "冷加载 (liii list) 自身" (- (current-second) t6))

(newline)
(display "=== 测试完成 ===")
(newline)
