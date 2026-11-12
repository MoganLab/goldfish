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

;; (hint scheme lazy) - (scheme lazy) 的类型接口
;;
;; promise 为记录类型，类型名取记录谓词 promise?。

(define-library (hint scheme lazy)
  (import (scheme base))
  (export hint-scheme/lazy)
  (begin

    ;; 语法形式：delay / delay-force 构造 promise
    (define hint-delay '((any? -> any?)))
    (define hint-delay-force '((any? -> any?)))

    ;; promise 求值
    (define hint-force '((any? -> any?)))

    ;; promise 构造与判定
    (define hint-make-promise '((any? -> promise?)))
    (define hint-promise? '((any? -> boolean?)))

    (define (hint-scheme/lazy name)
      (case name
       ((delay) hint-delay)
       ((delay-force) hint-delay-force)
       ((force) hint-force)
       ((make-promise) hint-make-promise)
       ((promise?) hint-promise?)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
