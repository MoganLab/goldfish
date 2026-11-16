;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai event turn-end)
  (import (scheme base) (liii base))
  (export pi-event-turn-end pi-event-turn-end? pi-event-turn-end-type
    pi-event-turn-end-message pi-event-turn-end-tool-results
  ) ;export
  (begin

    (define*-record-type <pi-event-turn-end>
      (pi-event-turn-end message (tool-results #()))
      pi-event-turn-end?
      (message pi-event-turn-end-message)
      (tool-results pi-event-turn-end-tool-results)
    ) ;define*-record-type

    (define (pi-event-turn-end-type ev)
      "turn_end"
    ) ;define

  ) ;begin
) ;define-library
