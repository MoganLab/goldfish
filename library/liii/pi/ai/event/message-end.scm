;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai event message-end)
  (import (scheme base) (liii base))
  (export pi-event-message-end pi-event-message-end?
    pi-event-message-end-type pi-event-message-end-message
  ) ;export
  (begin

    (define*-record-type <pi-event-message-end>
      (pi-event-message-end message)
      pi-event-message-end?
      (message pi-event-message-end-message)
    ) ;define*-record-type

    (define (pi-event-message-end-type ev)
      "message_end"
    ) ;define

  ) ;begin
) ;define-library
