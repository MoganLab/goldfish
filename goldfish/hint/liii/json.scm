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

;; (hint liii json) - (liii json) 的类型接口

(define-library (hint liii json)
  (import (scheme base))
  (export hint-liii/json)
  (begin

    ;; string->json : (string? -> any?)
    (define hint-string->json '((string? -> any?)))

    ;; json-object? : (any? -> boolean?)
    (define hint-json-object? '((any? -> boolean?)))

    ;; json-ref 的多键路径为可变参数，登记 2、3 个参数的形式
    (define hint-json-ref '((any? string? -> any?)
                            (any? string? string? -> any?)))

    ;; json-contains-key? : (any? string? -> boolean?)
    (define hint-json-contains-key? '((any? string? -> boolean?)))

    ;; json-set : (any? string? any? -> any?)
    (define hint-json-set '((any? string? any? -> any?)))

    ;; json-push : (any? string? any? -> any?)
    (define hint-json-push '((any? string? any? -> any?)))

    ;; json-keys : (any? -> list?)
    (define hint-json-keys '((any? -> list?)))

    (define (hint-liii/json name)
      (case name
       ((string->json) hint-string->json)
       ((json-object?) hint-json-object?)
       ((json-ref) hint-json-ref)
       ((json-contains-key?) hint-json-contains-key?)
       ((json-set) hint-json-set)
       ((json-push) hint-json-push)
       ((json-keys) hint-json-keys)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
