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

(define-library (liii pi gemini)
  (import (scheme base)
    (liii pi gemini core)
    (liii pi gemini request)
    (liii pi gemini stream)
    (liii pi gemini stream-parse)
  ) ;import
  (export gemini-map-finish-reason gemini-map-usage
    gemini-normalize-tool-call-id gemini-generate-tool-call-id
    gemini-build-contents gemini-build-request-body
    gemini-build-tool-declarations gemini-parse-response
    gemini-is-retriable-error? gemini-request gemini-request-stream
    format-failed-stream-msg
  ) ;export
) ;define-library
