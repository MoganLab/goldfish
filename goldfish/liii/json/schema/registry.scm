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

(define-library (liii json schema registry)
  (import (scheme base)
    (liii json)
    (liii json schema data)
    (liii json schema model)
    (liii error)
  ) ;import
  (export make-json-schema-registry json-schema-registry?
    json-schema-registry-add json-schema-registry-add-many
  ) ;export
  (begin
    (define (make-json-schema-registry)
      (make-registry '())
    ) ;define
    (define (check-resource! schema)
      (if (or (json-boolean? schema) (json-object? schema))
        schema
        (error 'json-schema-schema-error "Schema must be an object or boolean" schema)
      ) ;if
    ) ;define
    (define (json-schema-registry-add registry uri schema . rest)
      (unless (json-schema-registry? registry)
        (type-error "Expected a JSON Schema registry" registry)
      ) ;unless
      (unless (string? uri)
        (type-error "Resource URI must be a string" uri)
      ) ;unless
      (check-resource! schema)
      (if (assoc uri (registry-resources registry))
        (error 'json-schema-options-error "Resource URI is already registered" uri)
        (make-registry (cons (cons uri schema) (registry-resources registry)))
      ) ;if
    ) ;define
    (define (json-schema-registry-add-many registry resources)
      (let loop
        ((current registry) (rest resources))
        (if (null? rest)
          current
          (let ((resource (car rest)))
            (loop (json-schema-registry-add current (car resource) (cdr resource))
              (cdr rest)
            ) ;loop
          ) ;let
        ) ;if
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
