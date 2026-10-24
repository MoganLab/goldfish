;;
;; Ported from pi-code-review (https://github.com/MattDevy/pi-extensions)
;; Original work Copyright (c) 2026 Matt Devy
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code review)
  (import (liii pi code review files)
    (liii pi code review language)
    (liii pi code review prompt)
    (liii pi code review parse)
    (liii pi code review format)
    (liii pi code review auto)
  ) ;import
  (export PI_REVIEW_MAX_FILES PI_REVIEW_MAX_FILE_CHARS PI_REVIEW_SEVERITIES
    pi-parse-changed-files pi-changed-files-command pi-collect-changed-files
    pi-review-file-records pi-detect-language pi-language-checklist
    pi-build-language-section pi-build-review-prompt pi-build-fallback-prompt
    pi-parse-review-findings pi-format-review-findings pi-build-review-injection
    pi-edited-files-from-tool-calls
  ) ;export
  (begin
  ) ;begin
) ;define-library
