;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code event session)
  (import (scheme base) (liii base) (liii uuid) (liii pi ai time))
  (export pi-event-session pi-event-session? pi-event-session-type
    pi-event-session-version pi-event-session-id pi-event-session-timestamp
    pi-event-session-cwd
  ) ;export
  (begin

    (define*-record-type <pi-event-session>
      (pi-event-session cwd
        (id (uuid7))
        (timestamp (pi-iso-8601-utc-now))
        (version 3)
      ) ;pi-event-session
      pi-event-session?
      (version pi-event-session-version)
      (id pi-event-session-id)
      (timestamp pi-event-session-timestamp)
      (cwd pi-event-session-cwd)
    ) ;define*-record-type

    (define (pi-event-session-type ev)
      "session"
    ) ;define

  ) ;begin
) ;define-library
