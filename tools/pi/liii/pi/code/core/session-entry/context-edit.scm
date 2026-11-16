;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code core session-entry context-edit)
  (import (scheme base) (liii base))
  (export pi-session-context-edit-entry pi-session-context-edit-entry?
    pi-session-context-edit-entry-id pi-session-context-edit-entry-parent-id
    pi-session-context-edit-entry-timestamp
    pi-session-context-edit-entry-target-id
    pi-session-context-edit-entry-replacement
  ) ;export
  (begin

    (define*-record-type <pi-session-context-edit-entry>
      (pi-session-context-edit-entry id
        parent-id
        timestamp
        target-id
        (replacement #f)
      ) ;pi-session-context-edit-entry
      pi-session-context-edit-entry?
      (id pi-session-context-edit-entry-id)
      (parent-id pi-session-context-edit-entry-parent-id)
      (timestamp pi-session-context-edit-entry-timestamp)
      (target-id pi-session-context-edit-entry-target-id)
      (replacement pi-session-context-edit-entry-replacement)
    ) ;define*-record-type

  ) ;begin
) ;define-library
