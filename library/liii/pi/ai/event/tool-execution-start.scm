;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai event tool-execution-start)
  (import (scheme base) (liii base))
  (export pi-event-tool-execution-start pi-event-tool-execution-start?
    pi-event-tool-execution-start-type
    pi-event-tool-execution-start-tool-call-id
    pi-event-tool-execution-start-tool-name pi-event-tool-execution-start-args
  ) ;export
  (begin

    (define*-record-type <pi-event-tool-execution-start>
      (pi-event-tool-execution-start tool-call-id tool-name args)
      pi-event-tool-execution-start?
      (tool-call-id pi-event-tool-execution-start-tool-call-id)
      (tool-name pi-event-tool-execution-start-tool-name)
      (args pi-event-tool-execution-start-args)
    ) ;define*-record-type

    (define (pi-event-tool-execution-start-type ev)
      "tool_execution_start"
    ) ;define

  ) ;begin
) ;define-library
