;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai message tool-result)
  (import (scheme base) (liii base) (liii pi ai time))
  (export pi-tool-result-message pi-tool-result-message?
    pi-tool-result-message-role pi-tool-result-message-tool-call-id
    pi-tool-result-message-tool-name pi-tool-result-message-content
    pi-tool-result-message-is-error pi-tool-result-message-timestamp
  ) ;export
  (begin

    (define*-record-type <pi-tool-result-message>
      (pi-tool-result-message tool-call-id
        tool-name
        content
        (is-error #f)
        (timestamp (pi-current-time-millis))
      ) ;pi-tool-result-message
      pi-tool-result-message?
      (tool-call-id pi-tool-result-message-tool-call-id)
      (tool-name pi-tool-result-message-tool-name)
      (content pi-tool-result-message-content)
      (is-error pi-tool-result-message-is-error)
      (timestamp pi-tool-result-message-timestamp)
    ) ;define*-record-type

    (define (pi-tool-result-message-role msg)
      "toolResult"
    ) ;define

  ) ;begin
) ;define-library
