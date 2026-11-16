;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code core session-entry state)
  (import (scheme base) (liii base))
  (export pi-session-state-entry pi-session-state-entry?
    pi-session-state-entry-type pi-session-state-entry-id
    pi-session-state-entry-parent-id pi-session-state-entry-timestamp
    pi-session-state-entry-fields
  ) ;export
  (begin

    (define*-record-type <pi-session-state-entry>
      (pi-session-state-entry type id parent-id timestamp (fields '()))
      pi-session-state-entry?
      (type pi-session-state-entry-type)
      (id pi-session-state-entry-id)
      (parent-id pi-session-state-entry-parent-id)
      (timestamp pi-session-state-entry-timestamp)
      (fields pi-session-state-entry-fields)
    ) ;define*-record-type

  ) ;begin
) ;define-library
