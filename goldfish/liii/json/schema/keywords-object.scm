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

(define-library (liii json schema keywords-object)
  (import (scheme base)
    (liii json)
    (liii json schema data)
    (liii json schema error)
    (liii json schema evaluated)
  ) ;import
  (export object-keyword? validate-object)
  (begin
    (define (object-keyword? keyword)
      (member keyword
        '("properties" "required" "additionalProperties" "propertyNames"
          "minProperties" "maxProperties" "dependentRequired" "dependentSchemas"
          "unevaluatedProperties" "patternProperties")
      ) ;member
    ) ;define
    ;; pattern 类关键字按注解忽略（无原生正则后端）：
    ;; patternProperties 不再匹配任何键，见 evaluator 的 skipped-keyword?。
    (define (pattern-key? patterns key)
      #f
    ) ;define
    (define (additional-keys instance schema)
      (let ((properties (object-ref schema "properties" '(())))
            (patterns (object-ref schema "patternProperties" '(())))
           ) ;
        (let loop
          ((rest (entries instance)) (result '()))
          (if (null? rest)
            (reverse result)
            (loop (cdr rest)
              (if (or (object-has? properties (caar rest)) (pattern-key? patterns (caar rest)))
                result
                (cons (caar rest) result)
              ) ;if
            ) ;loop
          ) ;if
        ) ;let
      ) ;let
    ) ;define
    (define (fail message keyword value instance schema path schema-path)
      (list (validation-error message keyword value instance schema path schema-path '() #f)
      ) ;list
    ) ;define
    (define (evaluated-key? evaluated key)
      (let loop
        ((rest evaluated))
        (and (pair? rest)
          (or (string=? (key->string (car rest)) (key->string key)) (loop (cdr rest)))
        ) ;and
      ) ;let
    ) ;define
    (define (validate-object validator keyword value instance schema path
              schema-path root descend valid?
            ) ;validate-object
      (if (not (json-object? instance))
        '()
        (cond
         ((string=? keyword "properties")
          (collect-for-each
            (lambda (property)
              (let ((cell (object-cell instance (car property))))
                (if cell
                  (descend (cdr cell)
                    (cdr property)
                    (append path (list (car property)))
                    (append schema-path (list "properties" (car property)))
                    root
                  ) ;descend
                  '()
                ) ;if
              ) ;let
            ) ;lambda
            (entries value)
          ) ;collect-for-each
         ) ;
         ((string=? keyword "required")
          (collect-for-each
            (lambda (required)
              (if (object-has? instance required)
                '()
                (fail (string-append (key->string required) " is a required property")
                  keyword
                  value
                  instance
                  schema
                  path
                  schema-path
                ) ;fail
              ) ;if
            ) ;lambda
            (listify value)
          ) ;collect-for-each
         ) ;
         ((string=? keyword "additionalProperties")
          (collect-for-each
            (lambda (key)
              (let ((property (object-cell instance key)))
                (if (json-false? value)
                  (fail "Additional property is not allowed"
                    keyword
                    value
                    instance
                    schema
                    (append path (list key))
                    schema-path
                  ) ;fail
                  (descend (cdr property)
                    value
                    (append path (list key))
                    (append schema-path (list keyword))
                    root
                  ) ;descend
                ) ;if
              ) ;let
            ) ;lambda
            (additional-keys instance schema)
          ) ;collect-for-each
         ) ;
         ((string=? keyword "minProperties")
          (if (< (length (entries instance)) value)
            (fail "Too few properties" keyword value instance schema path schema-path)
            '()
          ) ;if
         ) ;
         ((string=? keyword "maxProperties")
          (if (> (length (entries instance)) value)
            (fail "Too many properties" keyword value instance schema path schema-path)
            '()
          ) ;if
         ) ;
         ((string=? keyword "dependentRequired")
          (collect-for-each
            (lambda (dependency)
              (if (object-has? instance (car dependency))
                (collect-for-each (lambda (needed)
                                    (if (object-has? instance needed)
                                      '()
                                      (fail "Dependent property is missing"
                                        keyword value instance schema path
                                        schema-path
                                      ) ;fail
                                    ) ;if
                                  ) ;lambda
                  (listify (cdr dependency))
                ) ;collect-for-each
                '()
              ) ;if
            ) ;lambda
            (entries value)
          ) ;collect-for-each
         ) ;
         ((string=? keyword "dependentSchemas")
          (collect-for-each
            (lambda (dependency)
              (if (object-has? instance (car dependency))
                (descend instance
                  (cdr dependency)
                  path
                  (append schema-path (list keyword (car dependency)))
                  root
                ) ;descend
                '()
              ) ;if
            ) ;lambda
            (entries value)
          ) ;collect-for-each
         ) ;
         ((string=? keyword "unevaluatedProperties")
          (let ((evaluated (evaluated-properties validator instance schema root valid?)))
            (collect-for-each
              (lambda (property)
                (if (evaluated-key? evaluated (car property))
                  '()
                  (if (json-false? value)
                    (fail "Unevaluated property is not allowed"
                      keyword
                      value
                      instance
                      schema
                      (append path (list (car property)))
                      schema-path
                    ) ;fail
                    (descend (cdr property)
                      value
                      (append path (list (car property)))
                      (append schema-path (list keyword))
                      root
                    ) ;descend
                  ) ;if
                ) ;if
              ) ;lambda
              (entries instance)
            ) ;collect-for-each
          ) ;let
         ) ;
         (else '())
        ) ;cond
      ) ;if
    ) ;define
  ) ;begin
) ;define-library
