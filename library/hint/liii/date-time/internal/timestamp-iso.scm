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

;; (hint liii date-time internal timestamp-iso) 的类型接口

(define-library (hint liii date-time internal timestamp-iso)
  (import (scheme base))
  (export hint-liii/date-time/internal/timestamp-iso)
  (begin

    ;; timestamp->iso-8601 : (any? -> string?)
    (define hint-timestamp->iso-8601 '((any? -> string?)))

    ;; iso-8601->timestamp : (string? -> timestamp?)
    (define hint-iso-8601->timestamp '((string? -> timestamp?)))

    (define (hint-liii/date-time/internal/timestamp-iso name)
      (case name
       ((timestamp->iso-8601) hint-timestamp->iso-8601)
       ((iso-8601->timestamp) hint-iso-8601->timestamp)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
