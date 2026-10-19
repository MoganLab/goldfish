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

(define-library (liii pi blueprint)
  (import (liii pi blueprint types)
    (liii pi blueprint graph)
    (liii pi blueprint state)
    (liii pi blueprint transition)
    (liii pi blueprint parse)
    (liii pi blueprint validate)
    (liii pi blueprint render)
    (liii pi blueprint prompt)
    (liii pi blueprint store)
    (liii pi blueprint gate)
    (liii pi blueprint runtime)
    (liii pi blueprint tools)
    (liii pi blueprint args)
  ) ;import
  (export pi-blueprint-null? pi-blueprint-json-normalize pi-blueprint-jref
    pi-blueprint-assoc-set pi-blueprint-vector->list pi-blueprint-list->vector
    pi-blueprint-task pi-blueprint-phase pi-blueprint-gate
    pi-blueprint-blueprint pi-blueprint-new-id pi-task-id pi-task-title
    pi-task-description pi-task-status pi-task-acceptance pi-task-file-targets
    pi-task-dependencies pi-task-notes pi-task-done? pi-phase-id pi-phase-title
    pi-phase-description pi-phase-status pi-phase-tasks pi-phase-gates
    pi-gate-type pi-gate-description pi-gate-command pi-gate-passed?
    pi-gate-last-checked pi-gate-error-message pi-blueprint-id
    pi-blueprint-objective pi-blueprint-status pi-blueprint-phases
    pi-blueprint-active-phase-id pi-blueprint-active-task-id
    pi-blueprint-all-tasks pi-blueprint-completed-task-ids
    pi-blueprint-flat-tasks pi-blueprint-find-task pi-blueprint-completed-ids
    pi-blueprint-blocked-ids pi-blueprint-ready? pi-blueprint-blocking-tasks
    pi-blueprint-detect-cycles pi-blueprint-topological-sort
    pi-blueprint-recompute-blocked pi-blueprint-get-next-task
    pi-blueprint-start-task pi-blueprint-complete-task pi-blueprint-skip-task
    pi-blueprint-set-gate pi-blueprint-advance-phase pi-blueprint-create
    pi-blueprint-abandon pi-blueprint-set-notes pi-extract-blueprint-json
    pi-parse-blueprint-plan pi-validate-blueprint-plan
    pi-blueprint-valid-gate-type? pi-render-blueprint-plan
    pi-format-blueprint-progress pi-render-next-task pi-blueprint-task-counts
    pi-build-blueprint-generate-prompt pi-build-blueprint-context
    pi-build-blueprint-turn-prompt pi-blueprint-turn-system-prompt
    pi-blueprint-default-state-path pi-blueprint-resolve-path
    pi-blueprint-plan-path pi-blueprint-history-path pi-blueprint-load-state
    pi-blueprint-save-state! pi-blueprint-append-history!
    pi-blueprint-history-entry pi-run-blueprint-gate
    pi-blueprint-gate-failure-summary pi-blueprint-supported-gate-type?
    pi-blueprint-current pi-blueprint-set-current! pi-blueprint-current-path
    pi-blueprint-set-current-path! pi-blueprint-load! pi-blueprint-save-current!
    pi-blueprint-append-event! pi-blueprint-clear! pi-blueprint-active?
    pi-blueprint-updated-with-history! pi-blueprint-update-schema
    pi-blueprint-status-schema pi-blueprint-next-schema
    pi-execute-blueprint-update pi-execute-blueprint-status
    pi-execute-blueprint-next pi-blueprint-update-definition
    pi-blueprint-status-definition pi-blueprint-next-definition
    pi-blueprint-args-defaults pi-blueprint-handle-flag pi-blueprint-validate
    pi-blueprint-local-flow? pi-blueprint-flag?
  ) ;export
  (begin
  ) ;begin
) ;define-library
