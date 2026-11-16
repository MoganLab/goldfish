;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai config record)
  (import (scheme base)
    (liii list)
    (liii string)
    (liii pi ai config retry)
    (liii pi ai config settings)
    (liii pi ai config credential)
    (liii pi ai config types)
  ) ;import
  (export pi-retry-config pi-retry-config? pi-retry-config-enabled
    pi-retry-config-max-retries pi-retry-config-base-delay-ms
    pi-retry-config-max-agent-delay-ms pi-retry-config-provider-max-retries
    pi-retry-config->alist pi-retry-config->json json->pi-retry-config
    pi-settings pi-settings? pi-settings-default-provider
    pi-settings-default-model pi-settings-default-thinking-level
    pi-settings-http-proxy pi-settings-retry pi-settings-default-tools
    pi-settings->alist pi-settings->json json->pi-settings pi-credential
    pi-credential? pi-credential-provider pi-settings-hide-thinking-block
    pi-settings-session-dir pi-credential-key pi-settings-model-thinking-level
    pi-settings-model-thinking-levels pi-settings-enable-skill-commands
    pi-credential-type pi-credential->alist pi-credential->json
    pi-model-metadata pi-model-metadata? pi-model-metadata-id
    pi-model-metadata-name pi-model-metadata-provider pi-model-metadata-base-url
    pi-model-metadata-context-window pi-model-metadata-max-tokens
    pi-model-metadata-matched? pi-model-metadata->alist pi-model-metadata->json
    pi-model-metadata-from-alist
  ) ;export
  (begin

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

    (define (pi-model-metadata->json m)
      (if (pair? m)
        m
        (let ((base
                (list (cons "id" (pi-model-metadata-id m))
                  (cons "name" (pi-model-metadata-name m))
                  (cons "provider" (pi-model-metadata-provider m))
                  (cons "baseUrl" (pi-model-metadata-base-url m))
                  (cons "matched" (if (pi-model-metadata-matched? m) 'true 'false))
                ) ;list
              ) ;base
              (cw (pi-model-metadata-context-window m))
              (mt (pi-model-metadata-max-tokens m))
             ) ;
          (append base
            (if cw (list (cons "contextWindow" cw)) '())
            (if mt (list (cons "maxTokens" mt)) '())
          ) ;append
        ) ;let
      ) ;if
    ) ;define

    ;; alist → record：会话从 JSON 载入后把旧形态的 model-meta 规范化为
    ;; record；已是 record 则原样返回。字段容错：缺失或 #f 时给安全默认值。
    (define (%meta-field alist key default)
      (let ((p (assoc key alist)))
        (if (and p (cdr p)) (cdr p) default)
      ) ;let
    ) ;define

    (define (pi-model-metadata-from-alist m)
      (if (pi-model-metadata? m)
        m
        (and (pair? m)
          (%make-pi-model-metadata (%meta-field m 'id "")
            (%meta-field m 'name "")
            (%meta-field m 'provider "")
            (%meta-field m 'base-url "")
            (%meta-field m 'context-window #f)
            (%meta-field m 'max-tokens #f)
            (%meta-field m 'matched? #t)
          ) ;%make-pi-model-metadata
        ) ;and
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
