;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai config types)
  (import (scheme base))
  (export %make-pi-retry-config pi-retry-config? pi-retry-config-enabled
    pi-retry-config-max-retries pi-retry-config-base-delay-ms
    pi-retry-config-max-agent-delay-ms pi-retry-config-provider-max-retries
    %make-pi-settings pi-settings? pi-settings-default-provider
    pi-settings-default-model pi-settings-default-thinking-level
    pi-settings-http-proxy pi-settings-retry pi-settings-default-tools
    pi-settings-session-dir %make-pi-credential pi-credential?
    pi-credential-provider pi-credential-key pi-credential-type
    %make-pi-model-metadata pi-model-metadata? pi-model-metadata-id
    pi-model-metadata-name pi-model-metadata-provider pi-model-metadata-base-url
    pi-model-metadata-context-window pi-model-metadata-max-tokens
    pi-model-metadata-matched?
  ) ;export
  (begin

    (define-record-type <pi-retry-config>
      (%make-pi-retry-config enabled max-retries base-delay-ms
        max-agent-delay-ms provider-max-retries
      ) ;%make-pi-retry-config
      pi-retry-config?
      (enabled pi-retry-config-enabled)
      (max-retries pi-retry-config-max-retries)
      (base-delay-ms pi-retry-config-base-delay-ms)
      (max-agent-delay-ms pi-retry-config-max-agent-delay-ms)
      (provider-max-retries pi-retry-config-provider-max-retries)
    ) ;define-record-type

    (define-record-type <pi-settings>
      (%make-pi-settings default-provider default-model default-thinking-level
        http-proxy retry default-tools session-dir
      ) ;%make-pi-settings
      pi-settings?
      (default-provider pi-settings-default-provider)
      (default-model pi-settings-default-model)
      (default-thinking-level pi-settings-default-thinking-level)
      (http-proxy pi-settings-http-proxy)
      (retry pi-settings-retry)
      (default-tools pi-settings-default-tools)
      (session-dir pi-settings-session-dir)
    ) ;define-record-type

    (define-record-type <pi-credential>
      (%make-pi-credential provider key type)
      pi-credential?
      (provider pi-credential-provider)
      (key pi-credential-key)
      (type pi-credential-type)
    ) ;define-record-type

    (define-record-type <pi-model-metadata>
      (%make-pi-model-metadata id name provider base-url context-window
        max-tokens matched?
      ) ;%make-pi-model-metadata
      pi-model-metadata?
      (id pi-model-metadata-id)
      (name pi-model-metadata-name)
      (provider pi-model-metadata-provider)
      (base-url pi-model-metadata-base-url)
      (context-window pi-model-metadata-context-window)
      (max-tokens pi-model-metadata-max-tokens)
      (matched? pi-model-metadata-matched?)
    ) ;define-record-type

  ) ;begin
) ;define-library
