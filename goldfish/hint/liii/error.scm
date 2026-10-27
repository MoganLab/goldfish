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

;; (hint liii error) - (liii error) 的类型接口
;;
;; 本库只提供类型签名数据，不导出 (liii error) 的任何过程，
;; 也不导入其他 hint 库，因此可以被独立加载。
;;
;; 目前只登记 (liii alist) 实际用到的过程。

(define-library (hint liii error)
  (import (scheme base))
  (export hint-liii/error)
  (begin

    ;; key-error : (key-error message . irritants)
    (define hint-key-error '((any? any? -> any?)))

    ;; type-error 为可变参数，登记 1、2 个参数的形式
    (define hint-type-error '((string? -> any?) (string? any? -> any?)))

    (define (hint-liii/error name)
      (case name
       ((key-error) hint-key-error)
       ((type-error) hint-type-error)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
