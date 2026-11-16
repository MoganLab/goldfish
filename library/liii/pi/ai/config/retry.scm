;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai config retry)
  (import (scheme base) (liii base) (liii error) (liii json))
  (export pi-retry-config pi-retry-config? pi-retry-config-enabled
    pi-retry-config-max-retries pi-retry-config-base-delay-ms
    pi-retry-config-max-agent-delay-ms pi-retry-config-provider-max-retries
    pi-retry-config->json json->pi-retry-config
  ) ;export
  (begin

    (define*-record-type <pi-retry-config>
      (pi-retry-config (enabled #t)
        (max-retries 3)
        (base-delay-ms 2000)
        (max-agent-delay-ms 60000)
        (provider-max-retries #f)
      ) ;pi-retry-config
      pi-retry-config?
      (enabled pi-retry-config-enabled)
      (max-retries pi-retry-config-max-retries)
      (base-delay-ms pi-retry-config-base-delay-ms)
      (max-agent-delay-ms pi-retry-config-max-agent-delay-ms)
      (provider-max-retries pi-retry-config-provider-max-retries)
    ) ;define*-record-type

    (define (pi-retry-config->json r)
      (unless (pi-retry-config? r)
        (type-error "pi-retry-config->json: expected pi-retry-config" r)
      ) ;unless
      (let ((base
              (list (cons "enabled" (if (pi-retry-config-enabled r) 'true 'false))
                (cons "maxRetries" (pi-retry-config-max-retries r))
                (cons "baseDelayMs" (pi-retry-config-base-delay-ms r))
                (cons "maxAgentDelayMs" (pi-retry-config-max-agent-delay-ms r))
              ) ;list
            ) ;base
            (provider-max (pi-retry-config-provider-max-retries r))
           ) ;
        (if provider-max
          (append base (list (cons "provider" (list (cons "maxRetries" provider-max)))))
          base
        ) ;if
      ) ;let
    ) ;define

    (define (json->pi-retry-config obj)
      (if (pi-retry-config? obj)
        obj
        (begin
          (unless (or (json-object? obj) (null? obj))
            (type-error "json->pi-retry-config: expected json object or pi-retry-config" obj)
          ) ;unless
          (if (null? obj)
            (pi-retry-config)
            (let ((raw-provider-max (json-ref obj "provider" "maxRetries")))
              (pi-retry-config (json-ref-boolean obj "enabled" #t)
                (json-ref-integer obj "maxRetries" 3)
                (json-ref-integer obj "baseDelayMs" 2000)
                (json-ref-integer obj "maxAgentDelayMs" 60000)
                (if (integer? raw-provider-max) raw-provider-max #f)
              ) ;pi-retry-config
            ) ;let
          ) ;if
        ) ;begin
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
