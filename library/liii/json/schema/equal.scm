;;
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
;;

(define-library (liii json schema equal)
  (import (scheme base) (liii json) (liii json schema data))
  (export json-equal?)
  (begin
    (define (json-equal? one two)
      (cond
       ((or (json-boolean? one) (json-boolean? two))
        (and (json-boolean? one)
          (json-boolean? two)
          (eq? (json-true? one) (json-true? two))
        ) ;and
       ) ;
       ((or (eq? one 'null) (eq? two 'null)) (and (eq? one 'null) (eq? two 'null)))
       ((and (number? one) (number? two)) (= one two))
       ((or (number? one) (number? two)) #f)
       ((and (string? one) (string? two)) (string=? one two))
       ((or (string? one) (string? two)) #f)
       ((and (vector? one) (vector? two))
        (and (= (vector-length one) (vector-length two))
          (let loop
            ((i 0))
            (or (= i (vector-length one))
              (and (json-equal? (vector-ref one i) (vector-ref two i)) (loop (+ i 1)))
            ) ;or
          ) ;let
        ) ;and
       ) ;
       ((or (vector? one) (vector? two)) #f)
       ((and (json-object? one) (json-object? two))
        (let ((a (entries one)) (b (entries two)))
          (and (= (length a) (length b))
            (let loop
              ((rest a))
              (or (null? rest)
                (let ((cell (object-cell two (caar rest))))
                  (and cell (json-equal? (cdar rest) (cdr cell)) (loop (cdr rest)))
                ) ;let
              ) ;or
            ) ;let
          ) ;and
        ) ;let
       ) ;
       ((or (json-object? one) (json-object? two)) #f)
       (else (equal? one two))
      ) ;cond
    ) ;define
  ) ;begin
) ;define-library
