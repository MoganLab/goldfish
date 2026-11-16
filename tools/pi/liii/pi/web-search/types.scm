;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi web-search types)
  (import (scheme base)
    (liii base)
    (liii error)
    (liii list)
    (liii path)
    (liii pi ai log)
    (liii json)
    (liii json schema)
    (liii json schema error)
  ) ;import
  (export make-pi-web-search-config pi-web-search-config?
    pi-web-search-config-provider pi-web-search-config-raw-model
    pi-web-search-config-raw-base-url pi-web-search-config-raw-proxy
    json->pi-web-search-config pi-web-search-config->json
  ) ;export
  (begin

    (define*-record-type <pi-web-search-config>
      (make-pi-web-search-config (provider #f) (model #f) (base-url #f) (proxy #f))
      pi-web-search-config?
      (provider pi-web-search-config-provider)
      (model pi-web-search-config-raw-model)
      (base-url pi-web-search-config-raw-base-url)
      (proxy pi-web-search-config-raw-proxy)
    ) ;define*-record-type

    (define %cached-schema #f)
    (define %cached-schema-path #f)

    (define (%web-search-schema)
      (unless %cached-schema
        (let ((schema-path
                (find (lambda (cand) (file-exists? (path->string cand)))
                  (map (lambda (d) (path-join d "liii/pi/web-search/resources/web-search.schema.json"))
                    *load-path*
                  ) ;map
                ) ;find
              ) ;schema-path
             ) ;
          (when schema-path
            (set! %cached-schema-path (path->string schema-path))
            (set! %cached-schema (string->json (path-read-text schema-path)))
          ) ;when
        ) ;let
      ) ;unless
      %cached-schema
    ) ;define

    (define (%format-schema-error err)
      (string-append "web-search.json schema validation error at "
        (json-schema-error-json-path err)
        " ("
        (json-schema-error-keyword err)
        "): "
        (json-schema-error-message err)
        (if %cached-schema-path (string-append " (schema: " %cached-schema-path ")") "")
      ) ;string-append
    ) ;define

    (define (%log-schema-errors errs)
      (for-each (lambda (err) (pi-log-error! (%format-schema-error err))) errs)
    ) ;define

    (define (json->pi-web-search-config data)
      (cond ((pi-web-search-config? data) data)
            ((or (not data) (null? data)) #f)
            ((json-object? data)
             (let* ((schema (%web-search-schema))
                    (errs (if schema (json-schema-errors data schema) '()))
                   ) ;
               (if (null? errs)
                 (make-pi-web-search-config (json-ref-string data "provider" #f)
                   (json-ref-string data "model" #f)
                   (json-ref-string data "baseUrl" #f)
                   (json-ref-string data "httpProxy" #f)
                 ) ;make-pi-web-search-config
                 (begin
                   (%log-schema-errors errs)
                   (error 'web-search-config-error (%format-schema-error (car errs)))
                 ) ;begin
               ) ;if
             ) ;let*
            ) ;
            (else (type-error "json->pi-web-search-config: expected json object, #f, or pi-web-search-config"
                    data
                  ) ;type-error
            ) ;else
      ) ;cond
    ) ;define

    (define (pi-web-search-config->json cfg)
      (unless (pi-web-search-config? cfg)
        (type-error "pi-web-search-config->json: expected pi-web-search-config" cfg)
      ) ;unless
      (let ((provider (pi-web-search-config-provider cfg))
            (model (pi-web-search-config-raw-model cfg))
            (base-url (pi-web-search-config-raw-base-url cfg))
            (proxy (pi-web-search-config-raw-proxy cfg))
           ) ;
        (append (if (string? provider) (list (cons "provider" provider)) '())
          (if (string? model) (list (cons "model" model)) '())
          (if (string? base-url) (list (cons "baseUrl" base-url)) '())
          (if (string? proxy) (list (cons "httpProxy" proxy)) '())
        ) ;append
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
