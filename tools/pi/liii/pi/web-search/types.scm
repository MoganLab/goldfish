;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi web-search types)
  (import (scheme base) (liii string) (liii list))
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

    (define (%jref obj key)
      (and (pair? obj)
        (let ((p
                (or (assoc key obj)
                  (if (symbol? key) (assoc (symbol->string key) obj) #f)
                  (if (string? key) (assoc (string->symbol key) obj) #f)
                ) ;or
              ) ;p
             ) ;
          (and (pair? p) (cdr p))
        ) ;let
      ) ;and
    ) ;define

    (define (%jstr obj . keys)
      (let loop
        ((rest keys))
        (if (null? rest)
          #f
          (let ((v (%jref obj (car rest))))
            (if
              (and (string? v) (not (string-null? (string-trim-both v))))
              v
              (loop (cdr rest))
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (json->pi-web-search-config data)
      (cond ((pi-web-search-config? data) data)
            ((pair? data)
             (make-pi-web-search-config (%jstr data 'provider)
               (%jstr data 'model)
               (%jstr data 'baseUrl 'baseURL 'base-url)
               (%jstr data 'httpProxy 'proxy 'http-proxy)
             ) ;make-pi-web-search-config
            ) ;
            (else #f)
      ) ;cond
    ) ;define

  ) ;begin
) ;define-library
