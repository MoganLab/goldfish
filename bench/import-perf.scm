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

;; import 性能基准测试（热路径：库已加载后的重复导入）
;; 覆盖：无修饰符导入、only/except/prefix/rename 修饰符、嵌套修饰符、多库导入
;; 冷加载（首次 load 库文件）的分层归因见 list-import-perf.scm
;;
;; 注：嵌套修饰符用例需要 [0112] 的 C 版 import；旧 Scheme 实现会直接报错
;; （r7rs-import-library-filename 无法解析嵌套 import set）。
;; 参考对比（10000 次，单次耗时）：旧 Scheme 版 → 新 C 版
;;   无修饰符   2.48μs → 1.46μs（1.7x）
;;   only 取5   1.97μs → 0.54μs（3.6x）
;;   except     4.59μs → 4.35μs（基本持平）
;;   prefix    10.96μs → 5.00μs（2.2x）
;;   rename     6.03μs → 4.24μs（1.4x）

(import (liii timeit) (liii list) (liii string) (scheme base))

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
  (display "=== import 性能测试（热路径，库已加载） ===\n\n")

  (bench "无修饰符 (import (liii list))          "
    (lambda () (import (liii list)))
    10000
  ) ;bench

  (bench "only 取5个名字                          "
    (lambda () (import (only (liii list) first second third fourth fifth)))
    10000
  ) ;bench

  (bench "except 排除5个名字                      "
    (lambda () (import (except (liii list) first second third fourth fifth)))
    10000
  ) ;bench

  (bench "prefix 加前缀                           "
    (lambda () (import (prefix (liii list) list:)))
    10000
  ) ;bench

  (bench "rename 重命名2个名字                    "
    (lambda ()
      (import (rename (liii list) (first list-first) (second list-second)))
    ) ;lambda
    10000
  ) ;bench

  (bench "嵌套 only(except(...))                  "
    (lambda () (import (only (except (liii list) first) second third)))
    10000
  ) ;bench

  (bench "嵌套 prefix(only(...))                  "
    (lambda () (import (prefix (only (liii list) first second) list:)))
    10000
  ) ;bench

  (bench "多库 (import (liii list) (liii string)) "
    (lambda () (import (liii list) (liii string)))
    10000
  ) ;bench

  (display "\n=== 测试完成 ===\n")
) ;define

(run-benchmarks)
