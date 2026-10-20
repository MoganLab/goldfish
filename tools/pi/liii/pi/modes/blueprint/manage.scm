;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi modes blueprint manage)
  (import (scheme base)
    (liii string)
    (liii pi args)
    (liii pi modes output)
    (liii pi modes flow)
    (liii pi modes blueprint common)
    (liii pi blueprint types)
    (liii pi blueprint graph)
    (liii pi blueprint state)
    (liii pi blueprint transition)
    (liii pi blueprint render)
    (liii pi blueprint prompt)
    (liii pi blueprint runtime)
  ) ;import
  (export run-blueprint-status-flow
    run-blueprint-next-flow
    run-blueprint-abandon-flow
  ) ;export
  (begin

    (define (run-blueprint-status-flow parsed mode)
      (let ((bp (pi-blueprint-load! (pi-bp-mode-state-path parsed))))
        (if (not bp)
          (begin
            (pi-bp-mode-out mode "No active blueprint.")
            0
          ) ;begin
          (begin
            (pi-bp-mode-out mode
              (string-append (pi-format-blueprint-progress bp)
                "\n\n"
                (pi-render-blueprint-plan bp)
              ) ;string-append
            ) ;pi-bp-mode-out
            0
          ) ;begin
        ) ;if
      ) ;let
    ) ;define

    (define (%no-next-message bp)
      (cond ((string=? (pi-blueprint-status bp) "completed") "Blueprint is complete.")
            ((string=? (pi-blueprint-status bp) "abandoned") "No active blueprint.")
            (else "No actionable tasks. Some may be blocked or awaiting verification.")
      ) ;cond
    ) ;define

    (define (%finish-next mode res-msgs)
      (let ((failure (pi-bp-mode-session-failure res-msgs)))
        (if failure
          (begin
            (pi-bp-mode-out mode failure)
            1
          ) ;begin
          (let ((code (if (eq? mode 'print) (handle-print-result (pi-last-message res-msgs)) 0))
               ) ;
            (let ((bp (pi-blueprint-current)))
              (when bp
                (pi-bp-mode-out mode (pi-format-blueprint-progress bp))
              ) ;when
            ) ;let
            code
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (run-blueprint-next-flow parsed cfg mode)
      (let* ((state-path (pi-bp-mode-state-path parsed))
             (bp (pi-blueprint-load! state-path))
            ) ;
        (cond ((not bp) (begin (pi-bp-mode-out mode "No active blueprint.") 0))
              ((not (string=? (pi-blueprint-status bp) "active"))
               (begin
                 (pi-bp-mode-out mode (%no-next-message bp))
                 0
               ) ;begin
              ) ;
              (else
                (let ((next (pi-blueprint-get-next-task bp)))
                  (if (not next)
                    (begin
                      (pi-bp-mode-out mode (%no-next-message bp))
                      0
                    ) ;begin
                    (let* ((task-id (pi-task-id next))
                           (bp1 (pi-blueprint-start-task bp task-id #f))
                           (prompt (pi-build-blueprint-turn-prompt next))
                           (bcfg (pi-bp-mode-config parsed cfg))
                          ) ;
                      (pi-blueprint-updated-with-history! bp1
                        "task_started"
                        task-id
                        (string-append "Task " task-id " -> in_progress")
                      ) ;pi-blueprint-updated-with-history!
                      (pi-bp-mode-out mode
                        (string-append (pi-render-next-task next)
                          "\n\n"
                          "Note: this task corresponds to a new three-stage workflow "
                          "(PLAN document, development, REVIEW). Assign the next repo task number."
                        ) ;string-append
                      ) ;pi-bp-mode-out
                      (run-session-flow parsed
                        bcfg
                        prompt
                        (pi-bp-mode-emit mode)
                        (lambda (res-msgs) (%finish-next mode res-msgs))
                      ) ;run-session-flow
                    ) ;let*
                  ) ;if
                ) ;let
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    (define (run-blueprint-abandon-flow parsed mode)
      (let* ((state-path (pi-bp-mode-state-path parsed))
             (bp (pi-blueprint-load! state-path))
            ) ;
        (if (not bp)
          (begin
            (pi-bp-mode-out mode "No active blueprint.")
            0
          ) ;begin
          (begin
            (pi-blueprint-updated-with-history! (pi-blueprint-abandon bp)
              "blueprint_abandoned"
              #f
              (pi-blueprint-objective bp)
            ) ;pi-blueprint-updated-with-history!
            (pi-bp-mode-out mode "Blueprint abandoned.")
            0
          ) ;begin
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
