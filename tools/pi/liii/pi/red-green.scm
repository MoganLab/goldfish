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

(define-library (liii pi red-green)
  (import (liii pi red-green state)
    (liii pi red-green classify)
    (liii pi red-green detect)
    (liii pi red-green gate)
    (liii pi red-green observe)
    (liii pi red-green prompt)
    (liii pi red-green report)
    (liii pi red-green tools)
  ) ;import
  (export pi-tdd-state? pi-tdd-state-phase pi-tdd-state-task
    pi-tdd-state-test-files pi-tdd-state-impl-files pi-tdd-state-last-test-run
    pi-tdd-state-phase-history pi-tdd-state-turn-index pi-tdd-state-warning
    pi-tdd-initial-state pi-tdd-idle-state pi-tdd-set-warning
    pi-tdd-add-test-file pi-tdd-add-impl-file pi-tdd-set-last-test-run
    pi-tdd-set-turn-index pi-tdd-stale-test-run? pi-tdd-advance-phase
    pi-tdd-current-state pi-tdd-set-current-state! pi-tdd-active? pi-tdd-start!
    pi-tdd-bump-turn! pi-tdd-reset! pi-tdd-glob-match? pi-tdd-language-of
    pi-classify-tdd-file pi-tdd-test-run-command? pi-tdd-parse-test-run-result
    pi-tdd-test-run-trigger pi-tdd-exit-code-from-text pi-tdd-write-gate
    pi-tdd-note-edited-file! pi-tdd-observe-tool! pi-tdd-red-phase-prompt
    pi-tdd-green-phase-prompt pi-tdd-refactor-phase-prompt
    pi-build-red-green-prompt pi-tdd-turn-system-prompt pi-format-tdd-summary
    pi-tdd-status-schema pi-tdd-advance-schema pi-tdd-reset-schema
    pi-execute-tdd-status pi-execute-tdd-advance pi-execute-tdd-reset
    pi-tdd-status-definition pi-tdd-advance-definition pi-tdd-reset-definition
  ) ;export
  (begin
  ) ;begin
) ;define-library
