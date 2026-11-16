;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai event agent-end)
  (import (scheme base) (liii base))
  (export pi-event-agent-end pi-event-agent-end? pi-event-agent-end-type
    pi-event-agent-end-messages pi-event-agent-end-will-retry
    pi-event-agent-end-cost pi-event-agent-end-savings
  ) ;export
  (begin

    (define*-record-type <pi-event-agent-end>
      (pi-event-agent-end messages (will-retry #f) (cost #f) (savings #f))
      pi-event-agent-end?
      (messages pi-event-agent-end-messages)
      (will-retry pi-event-agent-end-will-retry)
      (cost pi-event-agent-end-cost)
      (savings pi-event-agent-end-savings)
    ) ;define*-record-type

    (define (pi-event-agent-end-type ev)
      "agent_end"
    ) ;define

  ) ;begin
) ;define-library
