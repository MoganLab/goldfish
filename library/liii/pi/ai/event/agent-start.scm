;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai event agent-start)
  (import (scheme base) (liii base))
  (export pi-event-agent-start pi-event-agent-start? pi-event-agent-start-type)
  (begin

    (define*-record-type <pi-event-agent-start>
      (pi-event-agent-start)
      pi-event-agent-start?
    ) ;define*-record-type

    (define (pi-event-agent-start-type ev)
      "agent_start"
    ) ;define

  ) ;begin
) ;define-library
