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

;; gf fmt 的共享并发设施：底层复用 (liii goldtool parallel)。
;; 保留 fmt 历史接口名（set-fmt-jobs! / fmt-jobs）以兼容现有调用方。

(set! *load-path* (append (list "../common" "tools/common") *load-path*))

(define-library (liii goldfmt parallel)
  (import (scheme base) (liii goldtool parallel))
  (export pool-for-each run-one-task offenders-from count-status
    set-fmt-jobs! fmt-jobs
  ) ;export
  (begin

    (define (set-fmt-jobs! n)
      (set-goldtool-jobs! n)
    ) ;define

    (define (fmt-jobs)
      (goldtool-jobs)
    ) ;define

    ;; pool-for-each / run-one-task / offenders-from / count-status
    ;; 直接由 (liii goldtool parallel) 导入并导出。

  ) ;begin
) ;define-library
