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

;; (hint scheme complex) - (scheme complex) 的类型接口

(define-library (hint scheme complex)
  (import (scheme base))
  (export hint-scheme/complex)
  (begin

    ;; 复数构造
    (define hint-make-rectangular '((number? number? -> number?)))
    (define hint-make-polar '((number? number? -> number?)))

    ;; 分量与幅相
    (define hint-real-part '((number? -> number?)))
    (define hint-imag-part '((number? -> number?)))
    (define hint-magnitude '((number? -> number?)))
    (define hint-angle '((number? -> number?)))

    (define (hint-scheme/complex name)
      (case name
       ((make-rectangular) hint-make-rectangular)
       ((make-polar) hint-make-polar)
       ((real-part) hint-real-part)
       ((imag-part) hint-imag-part)
       ((magnitude) hint-magnitude)
       ((angle) hint-angle)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
