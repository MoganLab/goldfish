;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai anthropic)
  (import (scheme base)
    (liii pi ai anthropic core)
    (liii pi ai anthropic request)
    (liii pi ai anthropic stream)
    (liii pi ai anthropic stream-parse)
  ) ;import
  (export anthropic-map-stop-reason anthropic-map-usage
    anthropic-build-tool-declarations anthropic-build-messages
    anthropic-build-request-body anthropic-parse-response
    anthropic-is-retriable-error? anthropic-build-tool-declarations-from-names
    anthropic-request anthropic-request-stream append-anthropic-stream-text
    handle-anthropic-delta stop-anthropic-current-block process-anthropic-sse
    make-anthropic-stream-decoder anthropic-normalize-input
    anthropic-parse-tool-arguments
  ) ;export
) ;define-library
