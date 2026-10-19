;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(define-library (liii pi compact)
  (import (scheme base)
    (liii pi compact tokens)
    (liii pi compact cut)
    (liii pi compact serialize)
    (liii pi compact fileops)
    (liii pi compact prompt)
    (liii pi compact summary)
    (liii pi compact session)
  ) ;import
  (export pi-estimate-content-chars pi-estimate-message-tokens
    pi-context-token-estimate pi-should-compact? pi-compaction-default-settings
    pi-entry-message pi-entry-id pi-valid-cut-point? pi-find-cut-point
    pi-messages-to-summarize pi-truncate-for-summary pi-serialize-conversation
    pi-extract-file-ops pi-compute-file-lists pi-format-file-operations
    PI_COMPACTION_SUMMARY_PREFIX PI_COMPACTION_SUMMARY_SUFFIX
    PI_SUMMARIZATION_SYSTEM_PROMPT PI_SUMMARIZATION_PROMPT
    PI_UPDATE_SUMMARIZATION_PROMPT pi-compaction-summary-message
    pi-build-summarization-prompt pi-summarization-failure pi-generate-summary
    pi-read-session-entries pi-project-session-messages
    pi-last-compaction-summary pi-append-compaction-entry! pi-compact-session!
  ) ;export
  (begin
  ) ;begin
) ;define-library
