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

(define-library (liii packrat)
  (import (scheme base)
    (liii packrat position)
    (liii packrat error)
    (liii packrat result)
    (liii packrat combinator)
    (liii packrat parser)
  ) ;import
  (export
    ;; parse-result
    parse-result?
    parse-result-successful?
    parse-result-semantic-value
    parse-result-next
    parse-result-error
    make-result
    make-expected-result
    make-message-result
    merge-result-errors
    parse-error->parse-result

    ;; parse-results
    parse-results?
    parse-results-position
    parse-results-next
    parse-results-token-kind
    parse-results-token-value
    parse-results-base
    base-generator->results
    prepend-base
    prepend-semantic-value
    results->result

    ;; parse-error
    parse-error?
    parse-error-position
    parse-error-expected
    parse-error-messages
    make-error-expected
    make-error-message
    parse-error-empty?
    merge-parse-errors

    ;; parse-position
    make-parse-position
    parse-position?
    parse-position-file
    parse-position-line
    parse-position-column
    top-parse-position
    update-parse-position
    parse-position->string
    parse-position>?

    ;; combinators
    packrat-check-base
    packrat-check
    packrat-or
    packrat-unless

    ;; macros
    packrat-parser
  ) ;export
) ;define-library
