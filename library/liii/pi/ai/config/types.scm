;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai config types)
  (import (scheme base)
    (liii pi ai config retry)
    (liii pi ai config settings)
    (liii pi ai config credential)
    (liii pi ai config model-metadata)
  ) ;import
  (export pi-retry-config? pi-retry-config-enabled pi-retry-config-max-retries
    pi-retry-config-base-delay-ms pi-retry-config-max-agent-delay-ms
    pi-retry-config-provider-max-retries pi-settings?
    pi-settings-default-provider pi-settings-default-model
    pi-settings-default-thinking-level pi-settings-http-proxy pi-settings-retry
    pi-settings-default-tools pi-settings-session-dir
    pi-settings-hide-thinking-block pi-settings-enable-skill-commands
    pi-settings-model-thinking-levels pi-credential? pi-credential-provider
    pi-credential-key pi-credential-type pi-model-metadata? pi-model-metadata-id
    pi-model-metadata-name pi-model-metadata-provider pi-model-metadata-base-url
    pi-model-metadata-context-window pi-model-metadata-max-tokens
    pi-model-metadata-matched?
  ) ;export
  (begin
    #f
  ) ;begin
) ;define-library
