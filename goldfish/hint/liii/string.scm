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

;; (hint liii string) - (liii string) 的类型接口

(define-library (hint liii string)
  (import (scheme base))
  (export hint-liii/string)
  (begin

    ;; string-starts? : (string? string? -> boolean?)
    (define hint-string-starts? '((string? string? -> boolean?)))

    ;; string-ends? : (string? string? -> boolean?)
    (define hint-string-ends? '((string? string? -> boolean?)))

    ;; string-contains? : (string? string? -> boolean?)
    (define hint-string-contains? '((string? string? -> boolean?)))

    ;; string-split : (string? string? -> list?)
    (define hint-string-split '((string? string? -> list?)))

    ;; string-replace : (string? string? string? -> string?)
    (define hint-string-replace '((string? string? string? -> string?)))

    (define (hint-liii/string name)
      (case name
       ((string-starts?) hint-string-starts?)
       ((string-ends?) hint-string-ends?)
       ((string-contains?) hint-string-contains?)
       ((string-split) hint-string-split)
       ((string-replace) hint-string-replace)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
