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

(define-library (liii pi anthropic)
  (import (scheme base)
    (liii pi anthropic core)
    (liii pi anthropic request)
    (liii pi anthropic stream)
    (liii pi anthropic stream-parse)
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
