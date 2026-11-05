;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai config record)
  (import (scheme base) (liii string) (liii pi ai config types))
  (export pi-retry-config pi-retry-config? pi-retry-config-enabled
    pi-retry-config-max-retries pi-retry-config-base-delay-ms
    pi-retry-config-max-agent-delay-ms pi-retry-config-provider-max-retries
    pi-retry-config->alist pi-settings pi-settings? pi-settings-default-provider
    pi-settings-default-model pi-settings-default-thinking-level
    pi-settings-http-proxy pi-settings-retry pi-settings-default-tools
    pi-settings->alist pi-credential pi-credential? pi-credential-provider
    pi-settings-hide-thinking-block pi-settings-session-dir pi-credential-key
    pi-settings-model-thinking-level pi-settings-model-thinking-levels
    pi-credential-type pi-credential->alist pi-model-metadata pi-model-metadata?
    pi-model-metadata-id pi-model-metadata-name pi-model-metadata-provider
    pi-model-metadata-base-url pi-model-metadata-context-window
    pi-model-metadata-max-tokens pi-model-metadata-matched?
    pi-model-metadata->alist
  ) ;export
  (begin

    (define* (pi-retry-config (enabled #t)
               (max-retries 3)
               (base-delay-ms 2000)
               (max-agent-delay-ms 60000)
               (provider-max-retries #f)
             ) ;pi-retry-config
      (%make-pi-retry-config (if enabled #t #f)
        max-retries
        base-delay-ms
        max-agent-delay-ms
        provider-max-retries
      ) ;%make-pi-retry-config
    ) ;define*

    (define (pi-retry-config->alist r)
      (if (pair? r)
        r
        (list (cons 'enabled (pi-retry-config-enabled r))
          (cons 'max-retries (pi-retry-config-max-retries r))
          (cons 'base-delay-ms (pi-retry-config-base-delay-ms r))
          (cons 'max-agent-delay-ms (pi-retry-config-max-agent-delay-ms r))
          (cons 'provider-max-retries (pi-retry-config-provider-max-retries r))
        ) ;list
      ) ;if
    ) ;define

    (define* (pi-settings (default-provider "google")
               (default-model "gemini-3.8-flash")
               (default-thinking-level #f)
               (http-proxy #f)
               (retry #f)
               (default-tools #f)
               (session-dir #f)
               (hide-thinking-block #f)
               (model-thinking-levels '())
             ) ;pi-settings
      (%make-pi-settings default-provider
        default-model
        default-thinking-level
        http-proxy
        (or retry (pi-retry-config))
        default-tools
        session-dir
        (and hide-thinking-block #t)
        model-thinking-levels
      ) ;%make-pi-settings
    ) ;define*

    (define (pi-settings->alist s)
      (if (pair? s)
        s
        (list (cons 'default-provider (pi-settings-default-provider s))
          (cons 'default-model (pi-settings-default-model s))
          (cons 'default-thinking-level (pi-settings-default-thinking-level s))
          (cons 'http-proxy (pi-settings-http-proxy s))
          (cons 'retry (pi-settings-retry s))
          (cons 'default-tools (pi-settings-default-tools s))
          (cons 'session-dir (pi-settings-session-dir s))
          (cons 'hide-thinking-block (pi-settings-hide-thinking-block s))
          (cons 'model-thinking-levels (pi-settings-model-thinking-levels s))
        ) ;list
      ) ;if
    ) ;define

    ;; per-model 思考级别覆盖：键为 "provider/modelId"（对齐上游 modelThinkingLevels）
    ;; 本实现的 assoc 对字符串键不按 equal? 比较，故自行遍历
    (define (pi-settings-model-thinking-level settings provider model-id)
      (let* ((provider-str (if (symbol? provider) (symbol->string provider) provider))
             (wanted (string-append provider-str "/" model-id))
             (levels (pi-settings-model-thinking-levels settings))
            ) ;
        (let loop
          ((rest levels))
          (cond ((not (pair? rest)) #f)
                ((not (pair? (car rest))) (loop (cdr rest)))
                ((equal? (car (car rest)) wanted) (cdr (car rest)))
                (else (loop (cdr rest)))
          ) ;cond
        ) ;let
      ) ;let*
    ) ;define

    (define* (pi-credential provider key (type "api_key"))
      (%make-pi-credential (if (symbol? provider) (symbol->string provider) provider)
        key
        type
      ) ;%make-pi-credential
    ) ;define*

    (define (pi-credential->alist c)
      (if (pair? c)
        c
        (list (cons 'provider (pi-credential-provider c))
          (cons 'key (pi-credential-key c))
          (cons 'type (pi-credential-type c))
        ) ;list
      ) ;if
    ) ;define

    (define* (pi-model-metadata id
               name
               provider
               base-url
               context-window
               max-tokens
               (matched? #t)
             ) ;pi-model-metadata
      (%make-pi-model-metadata id
        name
        (if (symbol? provider) (symbol->string provider) provider)
        base-url
        context-window
        max-tokens
        (if matched? #t #f)
      ) ;%make-pi-model-metadata
    ) ;define*

    (define (pi-model-metadata->alist m)
      (if (pair? m)
        m
        (list (cons 'id (pi-model-metadata-id m))
          (cons 'name (pi-model-metadata-name m))
          (cons 'provider (pi-model-metadata-provider m))
          (cons 'base-url (pi-model-metadata-base-url m))
          (cons 'context-window (pi-model-metadata-context-window m))
          (cons 'max-tokens (pi-model-metadata-max-tokens m))
          (cons 'matched? (pi-model-metadata-matched? m))
        ) ;list
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
