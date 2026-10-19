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

(define-library (liii pi config record)
  (import (scheme base) (liii pi config types))
  (export pi-retry-config pi-retry-config? pi-retry-config-enabled
    pi-retry-config-max-retries pi-retry-config-base-delay-ms
    pi-retry-config-max-agent-delay-ms pi-retry-config-provider-max-retries
    pi-retry-config->alist pi-settings pi-settings? pi-settings-default-provider
    pi-settings-default-model pi-settings-default-thinking-level
    pi-settings-http-proxy pi-settings-retry pi-settings-default-tools
    pi-settings->alist pi-credential pi-credential? pi-credential-provider
    pi-credential-key pi-credential-type pi-credential->alist pi-model-metadata
    pi-model-metadata? pi-model-metadata-id pi-model-metadata-name
    pi-model-metadata-provider pi-model-metadata-base-url
    pi-model-metadata-context-window pi-model-metadata-max-tokens
    pi-model-metadata-matched? pi-model-metadata->alist
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
               (default-thinking-level "off")
               (http-proxy #f)
               (retry #f)
               (default-tools #f)
             ) ;pi-settings
      (%make-pi-settings default-provider
        default-model
        default-thinking-level
        http-proxy
        (or retry (pi-retry-config))
        default-tools
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
        ) ;list
      ) ;if
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
