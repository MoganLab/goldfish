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

;; (hint liii goldfmt format writer) - (liii goldfmt format writer) 的类型接口

(define-library (hint liii goldfmt format writer)
  (import (scheme base))
  (export hint-liii/goldfmt/format/writer)
  (begin

    ;; format-inline : (any? -> string?)
    (define hint-format-inline '((any? -> string?)))

    ;; try-inline : (any? -> (or #f string?))
    (define hint-try-inline '((any? -> (or #f string?))))

    ;; can-inline? : (any? -> boolean?)
    (define hint-can-inline? '((any? -> boolean?)))

    (define (hint-liii/goldfmt/format/writer name)
      (case name
       ((format-inline) hint-format-inline)
       ((try-inline) hint-try-inline)
       ((can-inline?) hint-can-inline?)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
