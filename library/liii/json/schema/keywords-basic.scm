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

(define-library (liii json schema keywords-basic)
  (import (scheme base)
    (liii json)
    (liii json schema data)
    (liii json schema equal)
    (liii json schema error)
    (liii unicode)
  ) ;import
  (export basic-keyword? validate-basic multiple-of?)
  (begin
    (define (basic-keyword? keyword)
      (member keyword
        '("type" "const" "enum" "minimum" "maximum" "exclusiveMinimum"
          "exclusiveMaximum" "multipleOf" "minLength" "maxLength")
      ) ;member
    ) ;define
    (define (type-matches? instance type)
      (let ((name (key->string type)))
        (cond ((string=? name "null") (eq? instance 'null))
              ((string=? name "boolean") (json-boolean? instance))
              ((string=? name "object") (json-object? instance))
              ((string=? name "array") (vector? instance))
              ((string=? name "string") (string? instance))
              ((string=? name "number")
               (and (number? instance) (real? instance) (not (json-boolean? instance)))
              ) ;
              ((string=? name "integer")
               (and (number? instance)
                 (real? instance)
                 (not (json-boolean? instance))
                 (or (integer? instance)
                   (guard (condition (#t #t))
                     (= instance (round instance))
                   ) ;guard
                 ) ;or
               ) ;and
              ) ;
              (else (error 'json-schema-schema-error
                      (string-append "Unknown JSON Schema type: " name)
                    ) ;error
              ) ;else
        ) ;cond
      ) ;let
    ) ;define
    (define (validate-type value instance schema path schema-path)
      (let loop
        ((rest (listify value)))
        (cond
         ((null? rest)
          (list (validation-error (string-append (value->display instance) " is not of the required type")
                  "type"
                  value
                  instance
                  schema
                  path
                  schema-path
                  '()
                  #f
                ) ;validation-error
          ) ;list
         ) ;
         ((type-matches? instance (car rest)) '())
         (else (loop (cdr rest)))
        ) ;cond
      ) ;let
    ) ;define
    (define (multiple-of? instance divisor)
      (if (or (not (number? divisor)) (zero? divisor))
        (error 'json-schema-schema-error "multipleOf requires a nonzero number")
        (let ((exact-result
                (guard (condition (#t #f))
                  (let ((quotient (/ (if (exact? instance) instance (inexact->exact instance))
                                    (if (exact? divisor) divisor (inexact->exact divisor))
                                  ) ;/
                        ) ;quotient
                       ) ;
                    (integer? quotient)
                  ) ;let
                ) ;guard
              ) ;exact-result
             ) ;
          (if exact-result
            #t
            (guard (fallback (#t #f))
              (and (inexact? instance)
                (> (abs instance) 1e+100)
                (let ((reciprocal (/ 1 divisor)))
                  (= reciprocal (round reciprocal))
                ) ;let
              ) ;and
            ) ;guard
          ) ;if
        ) ;let
      ) ;if
    ) ;define
    (define (number-bound keyword value instance schema path schema-path)
      (let ((fail? (cond ((string=? keyword "minimum") (< instance value))
                         ((string=? keyword "maximum") (> instance value))
                         ((string=? keyword "exclusiveMinimum") (<= instance value))
                         (else (>= instance value))
                   ) ;cond
            ) ;fail?
           ) ;
        (if fail?
          (list (validation-error (string-append (value->display instance) " violates " keyword)
                  keyword
                  value
                  instance
                  schema
                  path
                  schema-path
                  '()
                  #f
                ) ;validation-error
          ) ;list
          '()
        ) ;if
      ) ;let
    ) ;define
    (define (validate-basic keyword value instance schema path schema-path)
      (cond ((string=? keyword "type")
             (validate-type value instance schema path schema-path)
            ) ;
            ((string=? keyword "const")
             (if (json-equal? instance value)
               '()
               (list (validation-error "Value does not equal const"
                       keyword
                       value
                       instance
                       schema
                       path
                       schema-path
                       '()
                       #f
                     ) ;validation-error
               ) ;list
             ) ;if
            ) ;
            ((string=? keyword "enum")
             (if
               (let loop
                 ((rest (listify value)))
                 (and (pair? rest) (or (json-equal? instance (car rest)) (loop (cdr rest))))
               ) ;let
               '()
               (list (validation-error "Value is not in enum"
                       keyword
                       value
                       instance
                       schema
                       path
                       schema-path
                       '()
                       #f
                     ) ;validation-error
               ) ;list
             ) ;if
            ) ;
            ((member keyword '("minimum" "maximum" "exclusiveMinimum"
                               "exclusiveMaximum"))
             (if (or (not (number? instance)) (json-boolean? instance))
               '()
               (number-bound keyword value instance schema path schema-path)
             ) ;if
            ) ;
            ((string=? keyword "multipleOf")
             (if (and (number? instance)
                   (not (json-boolean? instance))
                   (not (multiple-of? instance value))
                 ) ;and
               (list (validation-error "Number is not a multiple"
                       keyword
                       value
                       instance
                       schema
                       path
                       schema-path
                       '()
                       #f
                     ) ;validation-error
               ) ;list
               '()
             ) ;if
            ) ;
            ((member keyword '("minLength" "maxLength"))
             (if
               (and (string? instance)
                 (if (string=? keyword "minLength")
                   (< (utf8-string-length instance) value)
                   (> (utf8-string-length instance) value)
                 ) ;if
               ) ;and
               (list (validation-error "String has an invalid length"
                       keyword
                       value
                       instance
                       schema
                       path
                       schema-path
                       '()
                       #f
                     ) ;validation-error
               ) ;list
               '()
             ) ;if
            ) ;
            (else '())
      ) ;cond
    ) ;define
  ) ;begin
) ;define-library
