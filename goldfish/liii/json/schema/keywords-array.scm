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

(define-library (liii json schema keywords-array)
  (import (scheme base)
    (liii json schema data)
    (liii json schema equal)
    (liii json schema error)
    (liii json schema evaluated)
  ) ;import
  (export array-keyword? validate-array)
  (begin
    (define (array-keyword? keyword)
      (member keyword
        '("prefixItems" "items" "minItems" "maxItems" "uniqueItems" "contains"
          "unevaluatedItems")
      ) ;member
    ) ;define
    (define (fail message keyword value instance schema path schema-path)
      (list (validation-error message keyword value instance schema path schema-path '() #f)
      ) ;list
    ) ;define
    (define (unique-vector? array)
      (let outer
        ((i 0))
        (or (= i (vector-length array))
          (let inner
            ((j (+ i 1)))
            (if (= j (vector-length array))
              (outer (+ i 1))
              (and (not (json-equal? (vector-ref array i) (vector-ref array j)))
                (inner (+ j 1))
              ) ;and
            ) ;if
          ) ;let
        ) ;or
      ) ;let
    ) ;define
    (define (prefix-errors value instance schema path schema-path root descend)
      (let loop
        ((i 0) (subschemas (listify value)) (errors '()))
        (if (or (= i (vector-length instance)) (null? subschemas))
          (reverse errors)
          (let ((child (descend (vector-ref instance i)
                         (car subschemas)
                         (append path (list i))
                         (append schema-path (list "prefixItems" i))
                         root
                       ) ;descend
                ) ;child
               ) ;
            (loop (+ i 1) (cdr subschemas) (append (reverse child) errors))
          ) ;let
        ) ;if
      ) ;let
    ) ;define
    (define (item-errors value instance schema path schema-path root descend)
      (let ((start (length (listify (object-ref schema "prefixItems" #())))))
        (let loop
          ((i start) (errors '()))
          (if (>= i (vector-length instance))
            (reverse errors)
            (let ((child (descend (vector-ref instance i)
                           value
                           (append path (list i))
                           (append schema-path (list "items"))
                           root
                         ) ;descend
                  ) ;child
                 ) ;
              (loop (+ i 1) (append (reverse child) errors))
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define
    (define (unevaluated-item-error value keyword instance schema path
              schema-path root descend index
            ) ;unevaluated-item-error
      (if (json-false? value)
        (list (validation-error "Unevaluated item is not allowed"
                keyword
                value
                instance
                schema
                (append path (list index))
                schema-path
                '()
                #f
              ) ;validation-error
        ) ;list
        (descend (vector-ref instance index)
          value
          (append path (list index))
          (append schema-path (list keyword))
          root
        ) ;descend
      ) ;if
    ) ;define
    (define (unevaluated-item-errors value keyword instance schema path
              schema-path root descend valid? evaluated
            ) ;unevaluated-item-errors
      (let loop
        ((index 0) (errors '()))
        (if (= index (vector-length instance))
          (reverse errors)
          (if (member index evaluated)
            (loop (+ index 1) errors)
            (let ((child (unevaluated-item-error value keyword instance schema
                           path schema-path root descend index
                         ) ;unevaluated-item-error
                  ) ;child
                 ) ;
              (loop (+ index 1) (append (reverse child) errors))
            ) ;let
          ) ;if
        ) ;if
      ) ;let
    ) ;define
    (define (validate-array validator keyword value instance schema path
              schema-path root descend valid?
            ) ;validate-array
      (if (not (vector? instance))
        '()
        (cond ((string=? keyword "prefixItems")
               (prefix-errors value instance schema path schema-path root descend)
              ) ;
              ((string=? keyword "items")
               (item-errors value instance schema path schema-path root descend)
              ) ;
              ((string=? keyword "unevaluatedItems")
               (let ((evaluated (evaluated-items validator instance schema root valid?)))
                 (unevaluated-item-errors value keyword instance schema path
                   schema-path root descend valid? evaluated
                 ) ;unevaluated-item-errors
               ) ;let
              ) ;
              ((string=? keyword "minItems")
               (if (< (vector-length instance) value)
                 (fail "Array is too short" keyword value instance schema path schema-path)
                 '()
               ) ;if
              ) ;
              ((string=? keyword "maxItems")
               (if (> (vector-length instance) value)
                 (fail "Array is too long" keyword value instance schema path schema-path)
                 '()
               ) ;if
              ) ;
              ((string=? keyword "uniqueItems")
               (if (and (json-true? value) (not (unique-vector? instance)))
                 (fail "Array elements are not unique" keyword value instance
                   schema path schema-path
                 ) ;fail
                 '()
               ) ;if
              ) ;
              ((string=? keyword "contains")
               (let* ((minimum (object-ref schema "minContains" 1))
                      (maximum (object-ref schema "maxContains" (vector-length instance)))
                      (count
                        (let loop
                          ((i 0) (n 0))
                          (if (= i (vector-length instance))
                            n
                            (loop (+ i 1) (if (valid? (vector-ref instance i) value root) (+ n 1) n))
                          ) ;if
                        ) ;let
                      ) ;count
                     ) ;
                 (if (<= minimum count maximum)
                   '()
                   (fail "Wrong number of matching array items" keyword value
                     instance schema path schema-path
                   ) ;fail
                 ) ;if
               ) ;let*
              ) ;
              (else '())
        ) ;cond
      ) ;if
    ) ;define
  ) ;begin
) ;define-library
