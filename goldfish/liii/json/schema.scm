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


;;; JSON Schema 2020-12 validation for Goldfish Scheme.
(define-library (liii json schema)
  (import (scheme base)
    (liii json)
    (liii json schema cache)
    (liii json schema budget)
    (liii json schema core)
    (liii json schema data)
    (liii json schema model)
    (liii json schema dialect)
    (liii json schema error)
    (liii json schema evaluator)
    (liii json schema keywords)
    (liii json schema reference)
    (liii json schema meta)
    (liii json schema registry)
    (liii error)
  ) ;import
  (export json-schema-validate json-schema-valid? json-schema-errors
    json-schema-check-schema make-json-schema-validator json-schema-validator?
    json-schema-validator-validate json-schema-validator-valid?
    json-schema-validator-errors json-schema-error? json-schema-error-kind
    json-schema-error-message json-schema-error-keyword
    json-schema-error-keyword-value json-schema-error-instance
    json-schema-error-schema json-schema-error-path
    json-schema-error-schema-path json-schema-error-context
    json-schema-error-cause json-schema-error-absolute-path
    json-schema-error-absolute-schema-path json-schema-error-json-path
    make-json-schema-registry json-schema-registry? json-schema-registry-add
    json-schema-registry-add-many
  ) ;export
  (define (basic-check-schema! schema)
    (if (or (json-boolean? schema) (json-object? schema))
      schema
      (error 'json-schema-schema-error "Schema must be an object or boolean" schema)
    ) ;if
  ) ;define
  (define (parse-options rest)
    (cond ((null? rest) '())
          ((and (pair? rest) (null? (cdr rest))) (car rest))
          (else (error 'json-schema-options-error "Expected at most one options alist"))
    ) ;cond
  ) ;define
  (define (validate-options! options)
    (unless (list? options)
      (error 'json-schema-options-error "Options must be an alist" options)
    ) ;unless
    (for-each
      (lambda (cell)
        (unless (and (pair? cell)
                  (symbol? (car cell))
                  (member (car cell)
                    '(check-schema? formats format-checkers registry retrieve
                       max-errors max-depth max-steps max-references)
                  ) ;member
                ) ;and
          (error 'json-schema-options-error "Unknown or malformed option" cell)
        ) ;unless
      ) ;lambda
      options
    ) ;for-each
    (if (assoc 'draft options)
      (error 'json-schema-options-error
        "The draft option is not supported; only 2020-12 is available"
      ) ;error
    ) ;if
    (let ((limit (option-ref options 'max-errors #f))
          (registry (option-ref options 'registry #f))
          (retrieve (option-ref options 'retrieve #f))
          (formats (option-ref options 'formats #f))
          (checkers (option-ref options 'format-checkers '()))
         ) ;
      (unless (or (not limit) (and (integer? limit) (exact? limit) (> limit 0)))
        (error 'json-schema-options-error
          "max-errors must be #f or a positive integer"
          limit
        ) ;error
      ) ;unless
      (unless (or (not registry) (json-schema-registry? registry))
        (error 'json-schema-options-error
          "registry must contain a JSON Schema registry"
          registry
        ) ;error
      ) ;unless
      (unless (or (not retrieve) (procedure? retrieve))
        (error 'json-schema-options-error "retrieve must be #f or a procedure" retrieve)
      ) ;unless
      (unless (or (not formats)
                (eq? formats #t)
                (and (proper-list? formats) (every symbol? formats))
              ) ;or
        (error 'json-schema-options-error
          "formats must be #f, #t or a list of format names"
          formats
        ) ;error
      ) ;unless
      (unless
        (and (proper-list? checkers)
          (every
            (lambda (cell) (and (pair? cell) (symbol? (car cell)) (procedure? (cdr cell))))
            checkers
          ) ;every
        ) ;and
        (error 'json-schema-options-error
          "format-checkers must be an alist of (format . procedure)"
          checkers
        ) ;error
      ) ;unless
      (validate-budget-options! options)
    ) ;let
    options
  ) ;define
  (define (make-meta-registry)
    (let loop
      ((uris (json-schema-meta-uris)) (registry (make-json-schema-registry)))
      (if (null? uris)
        registry
        (loop (cdr uris)
          (json-schema-registry-add registry
            (car uris)
            (json-schema-meta-resource (car uris))
          ) ;json-schema-registry-add
        ) ;loop
      ) ;if
    ) ;let
  ) ;define
  (define (options-with-meta-resources options)
    (let loop
      ((uris (json-schema-meta-uris))
       (registry (option-ref options 'registry (make-json-schema-registry)))
      ) ;
      (if (null? uris)
        (cons (cons 'reference-cache (make-reference-cache))
          (cons (cons 'resource-budget (resource-budget-from-options options))
            (cons (cons 'registry registry) options)
          ) ;cons
        ) ;cons
        (loop (cdr uris)
          (if (assoc (car uris) (registry-resources registry))
            registry
            (json-schema-registry-add registry
              (car uris)
              (json-schema-meta-resource (car uris))
            ) ;json-schema-registry-add
          ) ;if
        ) ;loop
      ) ;if
    ) ;let
  ) ;define
  (define meta-schema-uri "https://json-schema.org/draft/2020-12/schema")
  (define (make-json-schema-validator schema . rest)
    (let ((options (parse-options rest)))
      (validate-options! options)
      (basic-check-schema! schema)
      (draft-for schema)
      (if (option-ref options 'check-schema? #f)
        (json-schema-check-schema schema options)
      ) ;if
      (make-validator schema (options-with-meta-resources options))
    ) ;let
  ) ;define
  (define (json-schema-check-schema schema . rest)
    (let ((options (parse-options rest)))
      (validate-options! options)
      (draft-for schema)
      (basic-check-schema! schema)
      (let* ((registry (make-meta-registry))
             (meta-schema (json-schema-meta-resource meta-schema-uri))
             (validator
               (make-json-schema-validator meta-schema `((registry . ,registry)))
             ) ;validator
             (old-meta-root (meta-validation-root))
             (errors (with-meta-validation-root meta-schema
                       (lambda () (json-schema-validator-errors validator schema))
                     ) ;with-meta-validation-root
             ) ;errors
            ) ;
        (set-meta-validation-root! old-meta-root)
        (if (null? errors) schema (error 'json-schema-schema-error (car errors)))
      ) ;let*
    ) ;let
  ) ;define
  (define (json-schema-validator-errors validator instance)
    (unless (json-schema-validator? validator)
      (type-error "Expected a JSON Schema validator" validator)
    ) ;unless
    (reset-dynamic-scope!)
    (let ((budget (validator-budget validator)))
      (when budget
        (reset-resource-budget! budget)
      ) ;when
    ) ;let
    (validate-node validator
      instance
      (validator-schema validator)
      '()
      '()
      (validator-schema validator)
    ) ;validate-node
  ) ;define
  (define (json-schema-validator-valid? validator instance)
    (let* ((options (validator-options validator))
           (first-options
             (cons '(max-errors . 1)
               (let loop
                 ((rest options))
                 (cond ((null? rest) '())
                       ((eq? (caar rest) 'max-errors) (cdr rest))
                       (else (cons (car rest) (loop (cdr rest))))
                 ) ;cond
               ) ;let
             ) ;cons
           ) ;first-options
           (first-validator (make-validator (validator-schema validator) first-options))
          ) ;
      (null? (json-schema-validator-errors first-validator instance))
    ) ;let*
  ) ;define
  (define (json-schema-validator-validate validator instance)
    (let ((errors (json-schema-validator-errors validator instance)))
      (if (null? errors) instance (error 'json-schema-validation-error (car errors)))
    ) ;let
  ) ;define
  (define (prepare-validator schema options default-check)
    (let ((merged (if (assoc 'check-schema? options)
                    options
                    (cons (cons 'check-schema? default-check) options)
                  ) ;if
          ) ;merged
         ) ;
      (make-json-schema-validator schema merged)
    ) ;let
  ) ;define
  (define (json-schema-errors instance schema . rest)
    (let* ((options (if (null? rest) '() (car rest)))
           (validator (prepare-validator schema options #t))
          ) ;
      (json-schema-validator-errors validator instance)
    ) ;let*
  ) ;define
  (define (json-schema-valid? instance schema . rest)
    (let* ((options (if (null? rest) '() (car rest)))
           (validator (prepare-validator schema options #t))
          ) ;
      (json-schema-validator-valid? validator instance)
    ) ;let*
  ) ;define
  (define (json-schema-validate instance schema . rest)
    (let* ((options (if (null? rest) '() (car rest)))
           (validator (prepare-validator schema options #t))
          ) ;
      (json-schema-validator-validate validator instance)
    ) ;let*
  ) ;define
) ;define-library
