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


(define-library (liii json schema evaluator)
  (import (scheme base)
    (liii json)
    (liii json schema budget)
    (liii json schema data)
    (liii json schema model)
    (liii json schema dialect)
    (liii json schema error)
    (liii json schema keywords)
    (liii json schema reference)
  ) ;import
  (export validate-node validator-budget)
  (begin
    (define (take-error-list values count)
      (if (or (zero? count) (null? values))
        '()
        (cons (car values) (take-error-list (cdr values) (- count 1)))
      ) ;if
    ) ;define
    (define (uri-suffix? uri suffix)
      (let ((uri-length (string-length uri)) (suffix-length (string-length suffix)))
        (and (>= uri-length suffix-length)
          (string=? (substring uri (- uri-length suffix-length) uri-length) suffix)
        ) ;and
      ) ;let
    ) ;define
    (define (resource-root schema root)
      (if (not (and (json-object? schema) (object-has? schema "$id")))
        root
        (let ((id (object-ref schema "$id" #f))
              (root-id (and (json-object? root) (object-ref root "$id" #f)))
             ) ;
          (cond ((and (equal? schema root) (absolute-uri? id)) root)
                ((and (string? id)
                   (string? root-id)
                   (absolute-uri? root-id)
                   (uri-suffix? root-id id)
                 ) ;and
                 root
                ) ;
                ((and (string? id) (not (absolute-uri? id)))
                 (let ((absolute (resolve-resource-name id root)))
                   (map
                     (lambda (cell)
                       (if (string=? (key->string (car cell)) "$id") (cons (car cell) absolute) cell)
                     ) ;lambda
                     (entries schema)
                   ) ;map
                 ) ;let
                ) ;
                (else schema)
          ) ;cond
        ) ;let
      ) ;if
    ) ;define
    (define (run-keyword validator keyword value instance schema path
              schema-path current-root descend valid?
            ) ;run-keyword
      (call-with-dynamic-scope current-root
        (lambda ()
          (validate-keyword validator keyword value instance schema path
            schema-path current-root descend valid?
          ) ;validate-keyword
        ) ;lambda
      ) ;call-with-dynamic-scope
    ) ;define
    (define (skipped-keyword? keyword)
      (or (not (known-keyword? keyword))
        (member keyword '("then" "else" "minContains" "maxContains"))
      ) ;or
    ) ;define
    (define (validate-schema-entries validator schema instance path schema-path
              current-root descend valid? limit
            ) ;validate-schema-entries
      (let loop
        ((rest (entries schema)) (errors '()))
        (if
          (or (null? rest) (and limit (>= (length errors) limit)))
          (let ((ordered (reverse errors)))
            (if limit (take-error-list ordered limit) ordered)
          ) ;let
          (let* ((keyword (key->string (caar rest))) (value (cdar rest)))
            (if (skipped-keyword? keyword)
              (loop (cdr rest) errors)
              (let ((child (run-keyword validator keyword value instance schema
                             path schema-path current-root descend valid?
                           ) ;run-keyword
                    ) ;child
                   ) ;
                (loop (cdr rest) (append (reverse child) errors))
              ) ;let
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define
    ;; The resource budget bounds the work of a single validation run.
    ;; It travels with the validator options and is reset before every
    ;; run, so reusing a validator never carries a budget over.
    (define (validator-budget validator)
      (option-ref (validator-options validator) 'resource-budget #f)
    ) ;define
    (define (validate-node validator instance schema path schema-path root)
      (let* ((budget (validator-budget validator))
             (depth (if budget (enter-budget-depth! budget) #f))
            ) ;
        (when budget
          (count-budget-step! budget)
        ) ;when
        (let ((result (validate-node-body validator instance schema path schema-path root)))
          (when budget
            (leave-budget-depth! budget)
          ) ;when
          result
        ) ;let
      ) ;let*
    ) ;define
    (define (validate-node-body validator instance schema path schema-path root)
      (cond ((json-true? schema) '())
            ((json-false? schema)
             (list (make-validation-error 'validation
                     "False schema does not allow this value"
                     #f
                     #f
                     instance
                     schema
                     path
                     schema-path
                     '()
                     #f
                   ) ;make-validation-error
             ) ;list
            ) ;
            ((not (json-object? schema))
             (error 'json-schema-schema-error "Schema must be an object or boolean" schema)
            ) ;
            (else (unsupported-modern-keyword! schema)
              (define (descend next-instance next-schema next-path next-schema-path next-root)
                (validate-node validator next-instance next-schema next-path
                  next-schema-path next-root
                ) ;validate-node
              ) ;define
              (define (valid? next-instance next-schema next-root)
                (null? (validate-node validator next-instance next-schema '() '() next-root))
              ) ;define
              (let ((limit (option-ref (validator-options validator) 'max-errors #f))
                    (current-root (resource-root schema root))
                   ) ;
                (validate-schema-entries validator schema instance path
                  schema-path current-root descend valid? limit
                ) ;validate-schema-entries
              ) ;let
            ) ;else
      ) ;cond
    ) ;define
  ) ;begin
) ;define-library
