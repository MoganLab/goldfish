;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai models)
  (import (scheme base)
    (srfi srfi-1)
    (liii list)
    (liii string)
    (liii path)
    (liii json)
    (liii pi ai config types)
    (liii pi ai config record)
    (liii pi ai models list)
    (liii pi ai models builtin)
    (liii pi ai models route)
    (liii pi ai models catalog)
    (liii pi ai models meta)
    (liii pi ai models search)
  ) ;import
  (export pi-resolve-model-metadata pi-format-token-count pi-list-models
    pi-format-models-table PI_SUPPORTED_PROVIDERS PI_PROVIDER_BUILTIN_MODELS
    pi-provider-default-model pi-find-model-provider pi-provider-has-model?
    pi-route-provider-and-model pi-ensure-catalog-meta pi-builtin-augment-store
    pi-builtin-augment-all pi-catalog-provider-names pi-catalog-provider-models
    pi-catalog-merge-provider-models pi-fuzzy-match-model?
    pi-split-search-tokens pi-usable-models
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
      (and (path-exists? filepath)
        (catch #t (lambda () (string->json (path-read-text filepath))) (lambda args #f))
      ) ;and
    ) ;define

    (define (%models-as-list raw)
      (cond ((vector? raw) (vector->list raw))
            ((list? raw) raw)
            (else '())
      ) ;cond
    ) ;define

  ) ;begin
) ;define-library
