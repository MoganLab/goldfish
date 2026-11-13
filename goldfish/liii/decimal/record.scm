;;; liii/decimal/record.scm - decimal 记录类型与规范化
;;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;  http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(define-library (liii decimal record)
  (import (scheme base) (liii error) (liii bigint))

  (export decimal? make-decimal make-decimal-with-scale decimal-unscaled
    decimal-scale
  ) ;export

  (begin
    (define-record-type <decimal>
      (make-raw-decimal unscaled scale)
      decimal?
      (unscaled decimal-unscaled)
      (scale decimal-scale)
    ) ;define-record-type

    (define (make-decimal unscaled scale)
      (cond ((not (bigint? unscaled))
             (type-error "make-decimal in (liii decimal record): unscaled value must be a bigint"
               unscaled
             ) ;type-error
            ) ;
            ((not (integer? scale))
             (type-error "make-decimal in (liii decimal record): scale must be an integer"
               scale
             ) ;type-error
            ) ;
            ((bigint-zero? unscaled) (make-raw-decimal bigint-zero 0))
            (else
              (let loop
                ((value unscaled) (current-scale scale))
                (if
                  (or (<= current-scale 0)
                    (not (bigint-zero? (bigint-remainder value bigint-ten)))
                  ) ;or
                  (make-raw-decimal value current-scale)
                  (loop (bigint-divide value bigint-ten) (- current-scale 1))
                ) ;if
              ) ;let
            ) ;else
      ) ;cond
    ) ;define

    (define (make-decimal-with-scale unscaled scale)
      (cond ((not (bigint? unscaled))
             (type-error "make-decimal-with-scale in (liii decimal record): unscaled value must be a bigint"
               unscaled
             ) ;type-error
            ) ;
            ((not (integer? scale))
             (type-error "make-decimal-with-scale in (liii decimal record): scale must be an integer"
               scale
             ) ;type-error
            ) ;
            ((bigint-zero? unscaled) (make-raw-decimal bigint-zero scale))
            (else (make-raw-decimal unscaled scale))
      ) ;cond
    ) ;define
  ) ;begin
) ;define-library
