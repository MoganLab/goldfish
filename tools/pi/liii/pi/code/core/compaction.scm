;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code core compaction)
  (import (scheme base)
    (liii pi code core compaction tokens)
    (liii pi code core compaction cut)
    (liii pi code core compaction serialize)
    (liii pi code core compaction fileops)
    (liii pi code core compaction prompt)
    (liii pi code core compaction summary)
    (liii pi code core compaction session)
  ) ;import
  (export pi-estimate-content-chars pi-estimate-message-tokens
    pi-context-token-estimate pi-should-compact? pi-compaction-default-settings
    make-pi-compaction-settings pi-compaction-settings?
    pi-compaction-settings-enabled pi-compaction-settings-reserve-tokens
    pi-compaction-settings-keep-recent-tokens make-pi-context-token-estimate
    pi-context-token-estimate? pi-context-token-estimate-tokens
    pi-context-token-estimate-usage-tokens
    pi-context-token-estimate-trailing-tokens pi-entry-message pi-entry-id
    pi-valid-cut-point? pi-find-cut-point pi-messages-to-summarize
    pi-truncate-for-summary pi-serialize-conversation pi-content->text
    pi-extract-file-ops pi-compute-file-lists pi-format-file-operations
    make-pi-file-ops pi-file-ops? pi-file-ops-read pi-file-ops-written
    pi-file-ops-edited PI_COMPACTION_SUMMARY_PREFIX PI_COMPACTION_SUMMARY_SUFFIX
    PI_SUMMARIZATION_SYSTEM_PROMPT PI_SUMMARIZATION_PROMPT
    PI_UPDATE_SUMMARIZATION_PROMPT pi-compaction-summary-message
    pi-build-summarization-prompt pi-summarization-failure pi-generate-summary
    pi-read-session-entries pi-project-session-messages
    pi-last-compaction-summary pi-append-compaction-entry! pi-compact-session!
  ) ;export
  (begin
  ) ;begin
) ;define-library
