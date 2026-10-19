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

(define-library (liii pi simplify)
  (import (liii pi simplify ranges)
    (liii pi simplify scope)
    (liii pi simplify prompt)
    (liii pi simplify parse)
    (liii pi simplify format)
  ) ;import
  (export pi-parse-changed-ranges pi-diff-ranges-command
    pi-collect-simplify-files pi-build-scope-lines pi-format-range
    pi-build-simplify-prompt pi-build-simplify-apply-prompt
    pi-parse-simplify-suggestions pi-format-simplify-suggestions
  ) ;export
  (begin
  ) ;begin
) ;define-library
