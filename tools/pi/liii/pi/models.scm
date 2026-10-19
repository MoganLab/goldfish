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
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(define-library (liii pi models)
  (import (scheme base)
    (srfi srfi-1)
    (liii list)
    (liii string)
    (liii path)
    (liii json)
    (liii pi config types)
    (liii pi config record)
    (liii pi models list)
    (liii pi models builtin)
    (liii pi models route)
    (liii pi models catalog)
    (liii pi models meta)
    (liii pi models search)
  ) ;import
  (export pi-load-models-store pi-load-models-json
    pi-merge-models-json-into-store pi-resolve-model-metadata
    pi-format-token-count pi-list-models pi-format-models-table
    PI_SUPPORTED_PROVIDERS PI_PROVIDER_BUILTIN_MODELS pi-provider-default-model
    pi-find-model-provider pi-provider-has-model? pi-route-provider-and-model
    pi-ensure-catalog-meta pi-builtin-augment-store pi-builtin-augment-all
    pi-build-models-catalog pi-catalog-provider-names pi-catalog-provider-models
    pi-catalog-merge-provider-models pi-fuzzy-match-model?
    pi-split-search-tokens
  ) ;export
  (begin

    (define (%dict-ref obj key default-val)
      (if (not (pair? obj))
        default-val
        (let* ((str-key (if (symbol? key) (symbol->string key) key))
               (sym-key (if (string? key) (string->symbol key) key))
               (pair (or (assoc str-key obj) (assoc sym-key obj)))
              ) ;
          (if pair (cdr pair) default-val)
        ) ;let*
      ) ;if
    ) ;define

    (define (%read-json-file filepath)
      (and (path-exists? (path filepath))
        (catch #t
          (lambda () (string->json (path-read-text (path filepath))))
          (lambda args #f)
        ) ;catch
      ) ;and
    ) ;define

    (define (pi-load-models-store agent-dir)
      (let ((data
              (%read-json-file (path->string (path-join (path agent-dir) (path "models-store.json")))
              ) ;%read-json-file
            ) ;data
           ) ;
        (if (pair? data) data '())
      ) ;let
    ) ;define

    (define (pi-load-models-json agent-dir)
      (let ((data
              (%read-json-file (path->string (path-join (path agent-dir) (path "models.json")))
              ) ;%read-json-file
            ) ;data
           ) ;
        (if (pair? data) data '())
      ) ;let
    ) ;define

    (define (%models-as-list raw)
      (cond ((vector? raw) (vector->list raw))
            ((list? raw) raw)
            (else '())
      ) ;cond
    ) ;define

    (define (%provider-entry->model-ids p-entry)
      (map (lambda (m) (%dict-ref m "id" ""))
        (%models-as-list (%dict-ref (cdr p-entry) "models" '()))
      ) ;map
    ) ;define

    (define (%merge-provider-models store-entry custom-entry)
      (let* ((store-ids (%provider-entry->model-ids store-entry))
             (custom-list (%models-as-list (%dict-ref (cdr custom-entry) "models" '())))
             (new-models
               (filter
                 (lambda (m) (not (member (%dict-ref m "id" "") store-ids)))
                 custom-list
               ) ;filter
             ) ;new-models
             (store-models (%models-as-list (%dict-ref (cdr store-entry) "models" '())))
            ) ;
        (cons (car store-entry)
          (cons (cons "models" (append store-models new-models))
            (filter
              (lambda (kv)
                (let ((k (car kv)))
                  (not (or (equal? k "models") (equal? k 'models)))
                ) ;let
              ) ;lambda
              (cdr store-entry)
            ) ;filter
          ) ;cons
        ) ;cons
      ) ;let*
    ) ;define

    (define (%provider-name-str p-entry)
      (let ((n (car p-entry)))
        (if (string? n) n (symbol->string n))
      ) ;let
    ) ;define

    (define (%merge-existing-providers models-store providers)
      (map
        (lambda (p-entry)
          (let ((custom-pair
                  (or (assoc (%provider-name-str p-entry) providers)
                    (assoc (string->symbol (%provider-name-str p-entry)) providers)
                  ) ;or
                ) ;custom-pair
               ) ;
            (if custom-pair (%merge-provider-models p-entry custom-pair) p-entry)
          ) ;let
        ) ;lambda
        models-store
      ) ;map
    ) ;define

    (define (%new-provider-entries models-store providers)
      (let ((existing-names (map %provider-name-str models-store)))
        (filter
          (lambda (custom-entry)
            (not (member (%provider-name-str custom-entry) existing-names))
          ) ;lambda
          providers
        ) ;filter
      ) ;let
    ) ;define

    (define (pi-merge-models-json-into-store models-store models-json)
      (let ((providers (and (pair? models-json) (%dict-ref models-json "providers" '()))))
        (if (not (pair? providers))
          models-store
          (append (%merge-existing-providers models-store providers)
            (%new-provider-entries models-store providers)
          ) ;append
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
