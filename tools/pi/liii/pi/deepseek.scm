;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi deepseek)
  (import (scheme base) (liii pi anthropic))
  (export deepseek-map-stop-reason deepseek-map-usage
    deepseek-build-tool-declarations deepseek-build-messages
    deepseek-build-request-body deepseek-parse-response
    deepseek-is-retriable-error? deepseek-build-tool-declarations-from-names
    deepseek-request deepseek-request-stream
  ) ;export
  (begin

    (define deepseek-map-stop-reason anthropic-map-stop-reason)
    (define deepseek-map-usage anthropic-map-usage)
    (define deepseek-build-tool-declarations anthropic-build-tool-declarations)
    (define deepseek-build-messages anthropic-build-messages)
    (define deepseek-build-request-body anthropic-build-request-body)
    (define (deepseek-parse-response json-data . opt-status)
      (let ((status (if (pair? opt-status) (car opt-status) 200)))
        (anthropic-parse-response json-data status)
      ) ;let
    ) ;define
    (define (deepseek-is-retriable-error? status-code . opt-body)
      (let ((body (if (pair? opt-body) (car opt-body) "")))
        (anthropic-is-retriable-error? status-code body)
      ) ;let
    ) ;define
    (define deepseek-build-tool-declarations-from-names
      anthropic-build-tool-declarations-from-names
    ) ;define
    (define deepseek-request anthropic-request)
    (define deepseek-request-stream anthropic-request-stream)

  ) ;begin
) ;define-library
