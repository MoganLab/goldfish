;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai event agent-settled)
  (import (scheme base) (liii base))
  (export pi-event-agent-settled pi-event-agent-settled?
    pi-event-agent-settled-type pi-event-agent-settled-aborted
  ) ;export
  (begin

    (define*-record-type <pi-event-agent-settled>
      (pi-event-agent-settled (aborted #f))
      pi-event-agent-settled?
      (aborted pi-event-agent-settled-aborted)
    ) ;define*-record-type

    (define (pi-event-agent-settled-type ev)
      "agent_settled"
    ) ;define

  ) ;begin
) ;define-library
