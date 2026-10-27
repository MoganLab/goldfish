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

;; (hint liii goldcheck no-glue) - (liii goldcheck no-glue) 的类型接口

(define-library (hint liii goldcheck no-glue)
  (import (scheme base))
  (export hint-liii/goldcheck/no-glue)
  (begin

    ;; check-no-glue : (string? -> list?)
    (define hint-check-no-glue '((string? -> list?)))

    ;; check-no-glue-duplicates : (list? -> list?)
    (define hint-check-no-glue-duplicates '((list? -> list?)))

    (define (hint-liii/goldcheck/no-glue name)
      (case name
       ((check-no-glue) hint-check-no-glue)
       ((check-no-glue-duplicates) hint-check-no-glue-duplicates)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
