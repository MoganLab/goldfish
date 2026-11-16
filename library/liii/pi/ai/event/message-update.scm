;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai event message-update)
  (import (scheme base) (liii base))
  (export pi-event-message-update pi-event-message-update?
    pi-event-message-update-type pi-event-message-update-usage
    pi-event-message-update-assistant-message-event
  ) ;export
  (begin

    (define*-record-type <pi-event-message-update>
      (pi-event-message-update usage assistant-message-event)
      pi-event-message-update?
      (usage pi-event-message-update-usage)
      (assistant-message-event pi-event-message-update-assistant-message-event)
    ) ;define*-record-type

    (define (pi-event-message-update-type ev)
      "message_update"
    ) ;define

  ) ;begin
) ;define-library
