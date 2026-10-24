;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai kimi)
  (import (scheme base)
    (liii pi ai kimi core)
    (liii pi ai kimi msg)
    (liii pi ai kimi request)
    (liii pi ai kimi stream-parse)
    (liii pi ai kimi stream)
  ) ;import
  (export kimi-map-stop-reason kimi-map-usage kimi-parse-response
    kimi-is-retriable-error? kimi-build-tool-declarations kimi-build-messages
    kimi-build-request-body append-kimi-stream-text process-kimi-tool-call-delta
    finish-kimi-stream process-kimi-chunk make-kimi-stream-decoder
    extract-kimi-stream-error-message kimi-build-tool-declarations-from-names
    kimi-request kimi-build-endpoint kimi-build-headers kimi-build-proxy-alist
    kimi-request-stream
  ) ;export
) ;define-library
