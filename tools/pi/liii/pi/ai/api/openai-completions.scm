;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai api openai-completions)
  (import (scheme base)
    (liii pi ai api openai-completions core)
    (liii pi ai api openai-completions msg)
    (liii pi ai api openai-completions request)
    (liii pi ai api openai-completions stream-parse)
    (liii pi ai api openai-completions stream)
  ) ;import
  (export openai-map-stop-reason openai-map-usage openai-parse-response
    openai-is-retriable-error? openai-build-tool-declarations
    openai-build-messages openai-build-request-body append-openai-stream-text
    process-openai-tool-call-delta finish-openai-stream process-openai-chunk
    make-openai-stream-decoder extract-openai-stream-error-message
    openai-build-tool-declarations-from-names openai-request
    openai-build-endpoint openai-build-headers openai-build-proxy-alist
    openai-request-stream
  ) ;export
) ;define-library
