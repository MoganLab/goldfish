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

(define-library (liii pi code-review)
  (import (liii pi code-review files)
    (liii pi code-review language)
    (liii pi code-review prompt)
    (liii pi code-review parse)
    (liii pi code-review format)
    (liii pi code-review auto)
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
