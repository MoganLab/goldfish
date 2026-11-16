;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai config retry)
  (import (scheme base) (liii base) (liii error))
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

    (define (%json-dict-pair obj str-key sym-key)
      (let loop
        ((rest obj))
        (cond ((not (pair? rest)) #f)
              ((and (pair? (car rest))
                 (let ((k (car (car rest))))
                   (or (equal? k str-key) (equal? k sym-key))
                 ) ;let
               ) ;and
               (car rest)
              ) ;
              (else (loop (cdr rest)))
        ) ;cond
      ) ;let
    ) ;define

    (define (%json-dict-ref obj key default-val)
      (if (not (pair? obj))
        default-val
        (let* ((str-key (if (symbol? key) (symbol->string key) key))
               (sym-key (if (string? key) (string->symbol key) key))
               (pair (%json-dict-pair obj str-key sym-key))
              ) ;
          (if pair (cdr pair) default-val)
        ) ;let*
      ) ;if
    ) ;define

    (define (%normalize-json-bool val)
      (case val
       ((true) #t)
       ((false) #f)
       (else val)
      ) ;case
    ) ;define

    (define (json->pi-retry-config obj)
      (if (pi-retry-config? obj)
        obj
        (begin
          (unless (or (pair? obj) (null? obj))
            (type-error "json->pi-retry-config: expected json alist or pi-retry-config" obj)
          ) ;unless
          (let* ((provider-obj (%json-dict-ref obj "provider" '()))
                 (raw-provider-max (%json-dict-ref provider-obj "maxRetries" #f))
                 (provider-max (if (eq? raw-provider-max 'null) #f raw-provider-max))
                ) ;
            (pi-retry-config (if (%normalize-json-bool (%json-dict-ref obj "enabled" #t)) #t #f)
              (%json-dict-ref obj "maxRetries" 3)
              (%json-dict-ref obj "baseDelayMs" 2000)
              (%json-dict-ref obj "maxAgentDelayMs" 60000)
              provider-max
            ) ;pi-retry-config
          ) ;let*
        ) ;begin
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
