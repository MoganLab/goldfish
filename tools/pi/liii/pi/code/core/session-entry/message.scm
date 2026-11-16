;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code core session-entry message)
  (import (scheme base) (liii base))
  (export pi-session-message-entry pi-session-message-entry?
    pi-session-message-entry-id pi-session-message-entry-parent-id
    pi-session-message-entry-timestamp pi-session-message-entry-message
  ) ;export
  (begin

    (define*-record-type <pi-session-message-entry>
      (pi-session-message-entry id parent-id timestamp message)
      pi-session-message-entry?
      (id pi-session-message-entry-id)
      (parent-id pi-session-message-entry-parent-id)
      (timestamp pi-session-message-entry-timestamp)
      (message pi-session-message-entry-message)
    ) ;define*-record-type

  ) ;begin
) ;define-library
