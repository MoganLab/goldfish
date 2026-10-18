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

(define-library (liii pi kimi)
  (import (scheme base)
    (liii pi kimi core)
    (liii pi kimi msg)
    (liii pi kimi request)
    (liii pi kimi stream-parse)
    (liii pi kimi stream)
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
