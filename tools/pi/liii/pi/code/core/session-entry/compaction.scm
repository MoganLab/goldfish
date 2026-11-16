;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code core session-entry compaction)
  (import (scheme base) (liii base))
  (export pi-session-compaction-entry pi-session-compaction-entry?
    pi-session-compaction-entry-id pi-session-compaction-entry-parent-id
    pi-session-compaction-entry-timestamp pi-session-compaction-entry-summary
    pi-session-compaction-entry-first-kept-entry-id
    pi-session-compaction-entry-tokens-before
    pi-session-compaction-entry-details
  ) ;export
  (begin

    (define*-record-type <pi-session-compaction-entry>
      (pi-session-compaction-entry id
        parent-id
        timestamp
        summary
        first-kept-entry-id
        tokens-before
        (details '(()))
      ) ;pi-session-compaction-entry
      pi-session-compaction-entry?
      (id pi-session-compaction-entry-id)
      (parent-id pi-session-compaction-entry-parent-id)
      (timestamp pi-session-compaction-entry-timestamp)
      (summary pi-session-compaction-entry-summary)
      (first-kept-entry-id pi-session-compaction-entry-first-kept-entry-id)
      (tokens-before pi-session-compaction-entry-tokens-before)
      (details pi-session-compaction-entry-details)
    ) ;define*-record-type

  ) ;begin
) ;define-library
