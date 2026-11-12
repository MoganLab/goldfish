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

;; (hint scheme inexact) - (scheme inexact) 的类型接口

(define-library (hint scheme inexact)
  (import (scheme base))
  (export hint-scheme/inexact)
  (begin

    ;; 三角函数与指数、对数
    (define hint-sin '((number? -> number?)))
    (define hint-cos '((number? -> number?)))
    (define hint-tan '((number? -> number?)))
    (define hint-atan '((number? -> number?) (number? number? -> number?)))
    (define hint-asin '((number? -> number?)))
    (define hint-acos '((number? -> number?)))
    (define hint-exp '((number? -> number?)))
    (define hint-log '((number? -> number?) (number? number? -> number?)))
    (define hint-sqrt '((number? -> number?)))

    ;; 非有限数判断
    (define hint-finite? '((number? -> boolean?)))
    (define hint-infinite? '((number? -> boolean?)))
    (define hint-nan? '((number? -> boolean?)))

    (define (hint-scheme/inexact name)
      (case name
       ((sin) hint-sin)
       ((cos) hint-cos)
       ((tan) hint-tan)
       ((atan) hint-atan)
       ((asin) hint-asin)
       ((acos) hint-acos)
       ((exp) hint-exp)
       ((log) hint-log)
       ((sqrt) hint-sqrt)
       ((finite?) hint-finite?)
       ((infinite?) hint-infinite?)
       ((nan?) hint-nan?)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
