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

;; (hint srfi srfi-13) - (srfi srfi-13) 的类型接口

(define-library (hint srfi srfi-13)
  (import (scheme base))
  (export hint-srfi/srfi-13)
  (begin

    ;; string-join 为可变参数，登记 1-3 个参数的形式
    (define hint-string-join
      '((list? -> string?)
        (list? string? -> string?)
        (list? string? symbol? -> string?))
    ) ;define

    ;; string-prefix? : (string? string? -> boolean?)
    (define hint-string-prefix? '((string? string? -> boolean?)))

    ;; string-suffix? : (string? string? -> boolean?)
    (define hint-string-suffix? '((string? string? -> boolean?)))

    (define (hint-srfi/srfi-13 name)
      (case name
       ((string-join) hint-string-join)
       ((string-prefix?) hint-string-prefix?)
       ((string-suffix?) hint-string-suffix?)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
