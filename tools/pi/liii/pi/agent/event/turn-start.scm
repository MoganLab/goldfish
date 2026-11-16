;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi agent event turn-start)
  (import (scheme base) (liii base))
  (export pi-event-turn-start pi-event-turn-start? pi-event-turn-start-type)
  (begin

    (define*-record-type <pi-event-turn-start>
      (pi-event-turn-start)
      pi-event-turn-start?
    ) ;define*-record-type

    (define (pi-event-turn-start-type ev)
      "turn_start"
    ) ;define

  ) ;begin
) ;define-library
