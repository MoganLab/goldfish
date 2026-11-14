;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi web-search types)
  (import (scheme base)
    (liii list)
    (liii path)
    (liii logging)
    (liii json)
    (liii json schema)
    (liii json schema error)
  ) ;import
  (export make-pi-web-search-config pi-web-search-config?
    pi-web-search-config-provider pi-web-search-config-raw-model
    pi-web-search-config-raw-base-url pi-web-search-config-raw-proxy
    json->pi-web-search-config
  ) ;export
  (begin

    (define-record-type <pi-web-search-config>
      (make-pi-web-search-config provider model base-url proxy)
      pi-web-search-config?
      (provider pi-web-search-config-provider)
      (model pi-web-search-config-raw-model)
      (base-url pi-web-search-config-raw-base-url)
      (proxy pi-web-search-config-raw-proxy)
    ) ;define-record-type

    (define %cached-schema #f)

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
            (set! %cached-schema (string->json (path-read-text schema-path)))
          ) ;when
        ) ;let
      ) ;unless
      %cached-schema
    ) ;define

    (define (%log-schema-errors errs)
      (for-each
        (lambda (err)
          (log-warning (string-append "web-search.json schema validation error at "
                         (json-schema-error-json-path err)
                         " ("
                         (json-schema-error-keyword err)
                         "): "
                         (json-schema-error-message err)
                       ) ;string-append
          ) ;log-warning
        ) ;lambda
        errs
      ) ;for-each
    ) ;define

    (define (json->pi-web-search-config data)
      (cond ((pi-web-search-config? data) data)
            ((pair? data)
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
                   #f
                 ) ;begin
               ) ;if
             ) ;let*
            ) ;
            (else #f)
      ) ;cond
    ) ;define

  ) ;begin
) ;define-library
