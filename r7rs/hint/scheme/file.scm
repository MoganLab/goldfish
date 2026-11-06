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

;; (hint scheme file) - (scheme file) 的类型接口

(define-library (hint scheme file)
  (import (scheme base))
  (export hint-scheme/file)
  (begin

    ;; open-input-file : (string? -> input-port?)
    (define hint-open-input-file '((string? -> input-port?)))

    (define (hint-scheme/file name)
      (case name
       ((open-input-file) hint-open-input-file)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
