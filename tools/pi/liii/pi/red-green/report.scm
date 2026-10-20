;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi red-green report)
  (import (scheme base) (liii string) (liii list) (liii pi red-green state))
  (export pi-format-tdd-summary)
  (begin

    (define (%join-or-none items)
      (if (null? items) "(none)" (string-join items ", "))
    ) ;define

    (define (%phase-name phase)
      (symbol->string phase)
    ) ;define

    (define (%format-history history)
      (if (null? history)
        "(none)"
        (string-join
          (map
            (lambda (entry) (%phase-name (cdr (assoc 'phase entry))))
            history
          ) ;map
          " -> "
        ) ;string-join
      ) ;if
    ) ;define

    (define (%format-last-run run)
      (if (not run)
        "(none)"
        (string-append (number->string (cdr (assoc 'passed run)))
          " passed, "
          (number->string (cdr (assoc 'failed run)))
          " failed, "
          (number->string (cdr (assoc 'errors run)))
          " errors"
        ) ;string-append
      ) ;if
    ) ;define

    (define (pi-format-tdd-summary state)
      (let* ((phase (pi-tdd-state-phase state))
             (task (pi-tdd-state-task state))
             (warning (pi-tdd-state-warning state))
             (lines
               (list "## TDD Summary"
                 ""
                 (string-append "Phase: " (%phase-name phase))
                 (string-append "Task: " (if (string-null? task) "(none)" task))
                 (string-append "Test files: " (%join-or-none (pi-tdd-state-test-files state)))
                 (string-append "Implementation files: "
                   (%join-or-none (pi-tdd-state-impl-files state))
                 ) ;string-append
                 (string-append "Last test run: "
                   (%format-last-run (pi-tdd-state-last-test-run state))
                 ) ;string-append
                 (string-append "Phase history: "
                   (%format-history (pi-tdd-state-phase-history state))
                 ) ;string-append
               ) ;list
             ) ;lines
             (all-lines
               (if (and (string? warning) (not (string-null? warning)))
                 (append lines (list (string-append "Warning: " warning)))
                 lines
               ) ;if
             ) ;all-lines
            ) ;
        (string-append (string-join all-lines "\n") "\n")
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
