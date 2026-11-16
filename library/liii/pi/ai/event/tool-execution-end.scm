;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai event tool-execution-end)
  (import (scheme base) (liii base))
  (export pi-event-tool-execution-end pi-event-tool-execution-end?
    pi-event-tool-execution-end-type
    pi-event-tool-execution-end-tool-call-id
    pi-event-tool-execution-end-tool-name pi-event-tool-execution-end-result
    pi-event-tool-execution-end-is-error pi-event-tool-execution-end-duration-ms
  ) ;export
  (begin

    (define*-record-type <pi-event-tool-execution-end>
      (pi-event-tool-execution-end tool-call-id
        tool-name
        result
        (is-error #f)
        (duration-ms #f)
      ) ;pi-event-tool-execution-end
      pi-event-tool-execution-end?
      (tool-call-id pi-event-tool-execution-end-tool-call-id)
      (tool-name pi-event-tool-execution-end-tool-name)
      (result pi-event-tool-execution-end-result)
      (is-error pi-event-tool-execution-end-is-error)
      (duration-ms pi-event-tool-execution-end-duration-ms)
    ) ;define*-record-type

    (define (pi-event-tool-execution-end-type ev)
      "tool_execution_end"
    ) ;define

  ) ;begin
) ;define-library
