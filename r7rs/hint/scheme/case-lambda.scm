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
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; (hint scheme case-lambda) - (scheme case-lambda) 的类型接口
;;
;; case-lambda 是语法形式：接收子句列表，每个子句为 (形参列表 体)，
;; 体在参数个数匹配时求值。

(define-library (hint scheme case-lambda)
  (import (scheme base))
  (export hint-scheme/case-lambda)
  (begin

    ;; 语法形式：子句列表 + 体
    (define hint-case-lambda '((any? any? -> any?)))

    (define (hint-scheme/case-lambda name)
      (case name
       ((case-lambda) hint-case-lambda)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
