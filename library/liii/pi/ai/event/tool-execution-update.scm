;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai event tool-execution-update)
  (import (scheme base) (liii base))
  (export pi-event-tool-execution-update pi-event-tool-execution-update?
    pi-event-tool-execution-update-type
    pi-event-tool-execution-update-tool-call-id
    pi-event-tool-execution-update-tool-name pi-event-tool-execution-update-args
    pi-event-tool-execution-update-partial-result
  ) ;export
  (begin

    (define*-record-type <pi-event-tool-execution-update>
      (pi-event-tool-execution-update tool-call-id tool-name args partial-result)
      pi-event-tool-execution-update?
      (tool-call-id pi-event-tool-execution-update-tool-call-id)
      (tool-name pi-event-tool-execution-update-tool-name)
      (args pi-event-tool-execution-update-args)
      (partial-result pi-event-tool-execution-update-partial-result)
    ) ;define*-record-type

    (define (pi-event-tool-execution-update-type ev)
      "tool_execution_update"
    ) ;define

  ) ;begin
) ;define-library
