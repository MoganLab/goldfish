;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi red-green tools)
  (import (scheme base)
    (scheme char)
    (liii string)
    (liii list)
    (liii pi tools record)
    (liii pi red-green state)
  ) ;import
  (export pi-tdd-status-schema pi-tdd-advance-schema pi-tdd-reset-schema
    pi-execute-tdd-status pi-execute-tdd-advance pi-execute-tdd-reset
    pi-tdd-status-definition pi-tdd-advance-definition pi-tdd-reset-definition
  ) ;export
  (begin

    (define (%object-schema properties)
      (list (cons "type" "object") (cons "properties" properties))
    ) ;define

    (define (%reason-property)
      (list (cons "reason"
              (list (cons "type" "string")
                (cons "description" "Why you are manually changing the TDD state")
              ) ;list
            ) ;cons
      ) ;list
    ) ;define

    (define (pi-tdd-status-schema)
      (%object-schema (%reason-property))
    ) ;define

    (define (pi-tdd-advance-schema)
      (%object-schema (%reason-property))
    ) ;define

    (define (pi-tdd-reset-schema)
      (%object-schema (%reason-property))
    ) ;define

    (define (%join-or-none items)
      (if (null? items) "(none)" (string-join items ", "))
    ) ;define

    (define (%format-last-run state)
      (let ((run (pi-tdd-state-last-test-run state)))
        (if (not run)
          "Last run: (none)"
          (string-append "Last run: "
            (number->string (cdr (assoc 'passed run)))
            " passed, "
            (number->string (cdr (assoc 'failed run)))
            " failed, "
            (number->string (cdr (assoc 'errors run)))
            " errors"
            (if (pi-tdd-stale-test-run? state) " (STALE)" "")
          ) ;string-append
        ) ;if
      ) ;let
    ) ;define

    (define (%active-state)
      (let ((state (pi-tdd-current-state)))
        (and state (not (eq? (pi-tdd-state-phase state) 'idle)) state)
      ) ;let
    ) ;define

    (define (pi-execute-tdd-status args . opts)
      (let ((state (%active-state)))
        (if (not state)
          (pi-tool-result "TDD is not active." #f)
          (pi-tool-result
            (string-join
              (list (string-append "Phase: " (symbol->string (pi-tdd-state-phase state)))
                (string-append "Task: " (pi-tdd-state-task state))
                (string-append "Test files: " (%join-or-none (pi-tdd-state-test-files state)))
                (string-append "Impl files: " (%join-or-none (pi-tdd-state-impl-files state)))
                (%format-last-run state)
              ) ;list
              "\n"
            ) ;string-join
            #f
          ) ;pi-tool-result
        ) ;if
      ) ;let
    ) ;define

    (define (pi-execute-tdd-advance args . opts)
      (let ((state (%active-state)))
        (if (not state)
          (pi-tool-result "No active TDD session. Use --red-green to start one." #t)
          (let-values (((next warning) (pi-tdd-advance-phase state 'manual-advance)))
            (pi-tdd-set-current-state! next)
            (pi-tool-result
              (if warning
                warning
                (string-append "Advanced to "
                  (string-upcase (symbol->string (pi-tdd-state-phase next)))
                  " phase."
                ) ;string-append
              ) ;if
              #f
            ) ;pi-tool-result
          ) ;let-values
        ) ;if
      ) ;let
    ) ;define

    (define (%history-text state)
      (string-join
        (map
          (lambda (entry) (symbol->string (cdr (assoc 'phase entry))))
          (pi-tdd-state-phase-history state)
        ) ;map
        " -> "
      ) ;string-join
    ) ;define

    (define (pi-execute-tdd-reset args . opts)
      (let ((state (%active-state)))
        (if (not state)
          (pi-tool-result "TDD is already idle." #f)
          (let ((cycle (%history-text state)))
            (pi-tdd-set-current-state! (pi-tdd-idle-state))
            (pi-tool-result (string-append "TDD reset to idle. Cycle: "
                              (if (string-null? cycle) "(none)" cycle)
                            ) ;string-append
              #f
            ) ;pi-tool-result
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%make-definition name description execute snippet)
      (pi-tool-definition name
        description
        (pi-tdd-status-schema)
        execute
        snippet
        '("Use TDD tools to check, advance, or reset the current TDD session state.")
      ) ;pi-tool-definition
    ) ;define

    (define (pi-tdd-status-definition)
      (%make-definition "tdd_status"
        "Returns the current TDD phase, task, file history, test results, and staleness"
        pi-execute-tdd-status "Check the current TDD state and phase"
      ) ;%make-definition
    ) ;define

    (define (pi-tdd-advance-definition)
      (pi-tool-definition "tdd_advance"
        "Manually advance to the next TDD phase (escape hatch when auto-detection misses)"
        (pi-tdd-advance-schema)
        pi-execute-tdd-advance
        "Manually advance the TDD phase when auto-detection doesn't trigger"
        '("Use TDD tools to check, advance, or reset the current TDD session state.")
      ) ;pi-tool-definition
    ) ;define

    (define (pi-tdd-reset-definition)
      (pi-tool-definition "tdd_reset"
        "Reset TDD state to idle, recording the partial cycle"
        (pi-tdd-reset-schema)
        pi-execute-tdd-reset
        "Reset the TDD session to idle state"
        '("Use TDD tools to check, advance, or reset the current TDD session state.")
      ) ;pi-tool-definition
    ) ;define

  ) ;begin
) ;define-library
