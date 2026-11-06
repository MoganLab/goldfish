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

(define-library (liii json schema keywords-combo)
  (import (scheme base) (liii json schema data) (liii json schema error))
  (export combo-keyword? validate-combo)
  (begin
    (define (combo-keyword? keyword)
      (member keyword '("allOf" "anyOf" "oneOf" "not" "if"))
    ) ;define
    (define (validate-combo keyword value instance schema path schema-path root
              descend valid?
            ) ;validate-combo
      (cond
       ((string=? keyword "allOf")
        (let loop
          ((i 0) (rest (listify value)) (errors '()))
          (if (null? rest)
            (reverse errors)
            (let ((child (descend instance (car rest) path (append schema-path (list keyword i)) root)
                  ) ;child
                 ) ;
              (loop (+ i 1) (cdr rest) (append (reverse child) errors))
            ) ;let
          ) ;if
        ) ;let
       ) ;
       ((string=? keyword "anyOf")
        (let loop
          ((i 0) (rest (listify value)) (errors '()))
          (if (null? rest)
            (list (validation-error "Value does not match any schema"
                    keyword
                    value
                    instance
                    schema
                    path
                    schema-path
                    (reverse errors)
                    #f
                  ) ;validation-error
            ) ;list
            (let ((found (descend instance (car rest) path (append schema-path (list keyword i)) root)
                  ) ;found
                 ) ;
              (if (null? found) '() (loop (+ i 1) (cdr rest) (append (reverse found) errors)))
            ) ;let
          ) ;if
        ) ;let
       ) ;
       ((string=? keyword "oneOf")
        (let loop
          ((i 0) (rest (listify value)) (matches 0) (errors '()))
          (if (null? rest)
            (if (= matches 1)
              '()
              (list (validation-error "Value must match exactly one schema"
                      keyword
                      value
                      instance
                      schema
                      path
                      schema-path
                      (reverse errors)
                      #f
                    ) ;validation-error
              ) ;list
            ) ;if
            (let* ((found (descend instance (car rest) path (append schema-path (list keyword i)) root)
                   ) ;found
                   (next-matches (if (null? found) (+ matches 1) matches))
                  ) ;
              (loop (+ i 1) (cdr rest) next-matches (append (reverse found) errors))
            ) ;let*
          ) ;if
        ) ;let
       ) ;
       ((string=? keyword "not")
        (if (valid? instance value root)
          (list (validation-error "Value must not match the schema"
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
       ((string=? keyword "if")
        (let ((branch (if (valid? instance value root) "then" "else")))
          (let ((subschema (object-ref schema branch absent)))
            (if (eq? subschema absent)
              '()
              (descend instance subschema path (append schema-path (list branch)) root)
            ) ;if
          ) ;let
        ) ;let
       ) ;
       (else '())
      ) ;cond
    ) ;define
  ) ;begin
) ;define-library
