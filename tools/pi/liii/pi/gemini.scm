;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi gemini)
  (import (scheme base)
    (liii pi gemini core)
    (liii pi gemini request)
    (liii pi gemini stream)
    (liii pi gemini stream-parse)
    (liii pi gemini grounding)
  ) ;import
  (export gemini-map-finish-reason gemini-map-usage
    gemini-normalize-tool-call-id gemini-generate-tool-call-id
    gemini-build-contents gemini-build-request-body
    gemini-build-tool-declarations gemini-parse-response
    gemini-is-retriable-error? gemini-request gemini-request-stream
    format-failed-stream-msg gemini-build-search-tools
    gemini-inject-google-search gemini-extract-grounding-details
    gemini-grounding-search-queries gemini-grounding-search-results
    gemini-grounding-citations
  ) ;export
) ;define-library
