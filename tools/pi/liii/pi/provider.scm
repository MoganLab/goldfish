;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi provider)
  (import (scheme base) (liii pi gemini))
  (export gemini-map-finish-reason gemini-map-usage
    gemini-normalize-tool-call-id gemini-generate-tool-call-id
    gemini-build-contents gemini-build-request-body
  ) ;export
) ;define-library
