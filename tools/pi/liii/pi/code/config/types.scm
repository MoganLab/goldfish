;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code config types)
  (import (scheme base) (liii base) (liii error) (liii list) (liii json))
  (export make-pi-model-entry pi-model-entry? pi-model-entry-id
    pi-model-entry-name pi-model-entry-base-url pi-model-entry-api
    pi-model-entry-reasoning pi-model-entry-context-window
    pi-model-entry-max-tokens pi-model-entry-extras json->pi-model-entry
    pi-model-entry->json
  ) ;export
  (begin

    (define*-record-type <pi-model-entry>
      (make-pi-model-entry (id #f)
        (name #f)
        (base-url #f)
        (api #f)
        (reasoning 'unset)
        (context-window #f)
        (max-tokens #f)
        (extras '())
      ) ;make-pi-model-entry
      pi-model-entry?
      (id pi-model-entry-id)
      (name pi-model-entry-name)
      (base-url pi-model-entry-base-url)
      (api pi-model-entry-api)
      (reasoning pi-model-entry-reasoning)
      (context-window pi-model-entry-context-window)
      (max-tokens pi-model-entry-max-tokens)
      (extras pi-model-entry-extras)
    ) ;define*-record-type

    (define (%model-known-key? k)
      (let ((s (if (symbol? k) (symbol->string k) k)))
        (member s
          '("id" "name" "baseUrl" "api" "reasoning" "contextWindow" "maxTokens")
        ) ;member
      ) ;let
    ) ;define

    (define (%model-extras obj)
      (if (not (pair? obj))
        '()
        (filter
          (lambda (entry) (and (pair? entry) (not (%model-known-key? (car entry)))))
          obj
        ) ;filter
      ) ;if
    ) ;define

    (define (json->pi-model-entry obj)
      (if (pi-model-entry? obj)
        obj
        (begin
          (unless (or (json-object? obj) (null? obj))
            (type-error "json->pi-model-entry: expected json object or pi-model-entry" obj)
          ) ;unless
          (if (null? obj)
            (make-pi-model-entry)
            (let ((raw-reasoning (json-ref obj "reasoning")))
              (make-pi-model-entry (json-ref-string obj "id" #f)
                (json-ref-string obj "name" #f)
                (json-ref-string obj "baseUrl" #f)
                (json-ref-string obj "api" #f)
                (if (boolean? raw-reasoning) raw-reasoning 'unset)
                (json-ref-number obj "contextWindow" #f)
                (json-ref-number obj "maxTokens" #f)
                (%model-extras obj)
              ) ;make-pi-model-entry
            ) ;let
          ) ;if
        ) ;begin
      ) ;if
    ) ;define

    (define (pi-model-entry->json entry)
      (unless (pi-model-entry? entry)
        (type-error "pi-model-entry->json: expected pi-model-entry" entry)
      ) ;unless
      (let ((id (pi-model-entry-id entry))
            (name (pi-model-entry-name entry))
            (base-url (pi-model-entry-base-url entry))
            (api (pi-model-entry-api entry))
            (reasoning (pi-model-entry-reasoning entry))
            (cw (pi-model-entry-context-window entry))
            (mt (pi-model-entry-max-tokens entry))
            (extras (pi-model-entry-extras entry))
           ) ;
        (append (if (string? id) (list (cons "id" id)) '())
          (if (string? name) (list (cons "name" name)) '())
          (if (string? base-url) (list (cons "baseUrl" base-url)) '())
          (if (string? api) (list (cons "api" api)) '())
          (if (boolean? reasoning)
            (list (cons "reasoning" (if reasoning 'true 'false)))
            '()
          ) ;if
          (if (number? cw) (list (cons "contextWindow" cw)) '())
          (if (number? mt) (list (cons "maxTokens" mt)) '())
          (if (pair? extras) extras '())
        ) ;append
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
