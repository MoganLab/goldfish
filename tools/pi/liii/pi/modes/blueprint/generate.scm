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

(define-library (liii pi modes blueprint generate)
  (import (scheme base)
    (liii string)
    (liii pi args)
    (liii pi modes output)
    (liii pi modes flow)
    (liii pi modes blueprint common)
    (liii pi blueprint types)
    (liii pi blueprint parse)
    (liii pi blueprint validate)
    (liii pi blueprint render)
    (liii pi blueprint prompt)
    (liii pi blueprint runtime)
    (liii pi blueprint transition)
  ) ;import
  (export run-blueprint-generate-flow)
  (begin

    (define (%finish-generate mode objective state-path text res-msgs)
      (let ((failure (pi-bp-mode-session-failure res-msgs)))
        (if failure
          (begin
            (pi-bp-mode-out mode failure)
            1
          ) ;begin
          (let ((result
                  (catch #t
                    (lambda ()
                      (let ((plan (pi-parse-blueprint-plan text objective)))
                        (pi-validate-blueprint-plan plan)
                        plan
                      ) ;let
                    ) ;lambda
                    (lambda (tag . info) (cons 'error (pi-bp-mode-error-text tag info)))
                  ) ;catch
                ) ;result
               ) ;
            (if (and (pair? result) (eq? (car result) 'error))
              (begin
                (pi-bp-mode-out mode (string-append "Error: " (cdr result)))
                1
              ) ;begin
              (let* ((phases (pi-blueprint-vector->list (pi-blueprint-jref result "phases" #())))
                     (bp (pi-blueprint-create (pi-blueprint-new-id) objective "unknown" phases))
                    ) ;
                (pi-blueprint-set-current-path! state-path)
                (pi-blueprint-updated-with-history! bp "blueprint_created" #f objective)
                (pi-bp-mode-out mode
                  (string-append (pi-format-blueprint-progress bp)
                    "\n\n"
                    (pi-render-blueprint-plan bp)
                  ) ;string-append
                ) ;pi-bp-mode-out
                0
              ) ;let*
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (run-blueprint-generate-flow parsed cfg mode initial-msg)
      (let* ((objective (string-trim-both initial-msg))
             (state-path (pi-bp-mode-state-path parsed))
             (existing (pi-blueprint-load! state-path))
            ) ;
        (if (pi-blueprint-active? existing)
          (begin
            (pi-bp-mode-out mode
              (string-append "An active blueprint already exists: \""
                (pi-blueprint-objective existing)
                "\"\nUse --plan-abandon to discard it, or --plan-status for details."
              ) ;string-append
            ) ;pi-bp-mode-out
            1
          ) ;begin
          (let* ((bcfg (pi-bp-mode-config parsed cfg))
                 (prompt (pi-build-blueprint-generate-prompt objective))
                ) ;
            (run-session-flow parsed
              bcfg
              prompt
              (pi-bp-mode-emit mode)
              (lambda (res-msgs)
                (%finish-generate mode
                  objective
                  state-path
                  (pi-last-assistant-text res-msgs)
                  res-msgs
                ) ;%finish-generate
              ) ;lambda
            ) ;run-session-flow
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
