;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi agent event message-start)
  (import (scheme base) (liii base))
  (export pi-event-message-start pi-event-message-start?
    pi-event-message-start-type pi-event-message-start-message
  ) ;export
  (begin

    (define*-record-type <pi-event-message-start>
      (pi-event-message-start message)
      pi-event-message-start?
      (message pi-event-message-start-message)
    ) ;define*-record-type

    (define (pi-event-message-start-type ev)
      "message_start"
    ) ;define

  ) ;begin
) ;define-library
