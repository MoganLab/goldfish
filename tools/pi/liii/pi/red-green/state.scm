;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi red-green state)
  (import (scheme base) (liii string) (liii pi time))
  (export pi-tdd-state? pi-tdd-state-phase pi-tdd-state-task
    pi-tdd-state-test-files pi-tdd-state-impl-files pi-tdd-state-last-test-run
    pi-tdd-state-phase-history pi-tdd-state-turn-index pi-tdd-state-warning
    pi-tdd-initial-state pi-tdd-idle-state pi-tdd-set-warning
    pi-tdd-add-test-file pi-tdd-add-impl-file pi-tdd-set-last-test-run
    pi-tdd-set-turn-index pi-tdd-stale-test-run? pi-tdd-advance-phase
    pi-tdd-current-state pi-tdd-set-current-state! pi-tdd-active? pi-tdd-start!
    pi-tdd-bump-turn! pi-tdd-reset!
  ) ;export
  (begin

    (define-record-type <pi-tdd-state>
      (%make-pi-tdd-state phase task test-files impl-files last-test-run
        phase-history turn-index warning
      ) ;%make-pi-tdd-state
      pi-tdd-state?
      (phase pi-tdd-state-phase)
      (task pi-tdd-state-task)
      (test-files pi-tdd-state-test-files)
      (impl-files pi-tdd-state-impl-files)
      (last-test-run pi-tdd-state-last-test-run)
      (phase-history pi-tdd-state-phase-history)
      (turn-index pi-tdd-state-turn-index)
      (warning pi-tdd-state-warning)
    ) ;define-record-type

    (define (%phase-entry phase)
      (list (cons 'phase phase) (cons 'entered-at (pi-iso-8601-utc-now)))
    ) ;define

    (define (pi-tdd-initial-state task)
      (%make-pi-tdd-state 'red task '() '() #f (list (%phase-entry 'red)) 0 #f)
    ) ;define

    (define (pi-tdd-idle-state)
      (%make-pi-tdd-state 'idle "" '() '() #f '() 0 #f)
    ) ;define

    (define (%copy-state s phase task test-files impl-files last-test-run
              phase-history turn-index warning
            ) ;%copy-state
      (%make-pi-tdd-state phase task test-files impl-files last-test-run
        phase-history turn-index warning
      ) ;%make-pi-tdd-state
    ) ;define

    (define (pi-tdd-set-warning s warning)
      (%copy-state s
        (pi-tdd-state-phase s)
        (pi-tdd-state-task s)
        (pi-tdd-state-test-files s)
        (pi-tdd-state-impl-files s)
        (pi-tdd-state-last-test-run s)
        (pi-tdd-state-phase-history s)
        (pi-tdd-state-turn-index s)
        warning
      ) ;%copy-state
    ) ;define

    (define (pi-tdd-add-test-file s path)
      (if (member path (pi-tdd-state-test-files s))
        s
        (%copy-state s
          (pi-tdd-state-phase s)
          (pi-tdd-state-task s)
          (append (pi-tdd-state-test-files s) (list path))
          (pi-tdd-state-impl-files s)
          (pi-tdd-state-last-test-run s)
          (pi-tdd-state-phase-history s)
          (pi-tdd-state-turn-index s)
          (pi-tdd-state-warning s)
        ) ;%copy-state
      ) ;if
    ) ;define

    (define (pi-tdd-add-impl-file s path)
      (if (member path (pi-tdd-state-impl-files s))
        s
        (%copy-state s
          (pi-tdd-state-phase s)
          (pi-tdd-state-task s)
          (pi-tdd-state-test-files s)
          (append (pi-tdd-state-impl-files s) (list path))
          (pi-tdd-state-last-test-run s)
          (pi-tdd-state-phase-history s)
          (pi-tdd-state-turn-index s)
          (pi-tdd-state-warning s)
        ) ;%copy-state
      ) ;if
    ) ;define

    (define (pi-tdd-set-last-test-run s run)
      (%copy-state s
        (pi-tdd-state-phase s)
        (pi-tdd-state-task s)
        (pi-tdd-state-test-files s)
        (pi-tdd-state-impl-files s)
        run
        (pi-tdd-state-phase-history s)
        (pi-tdd-state-turn-index s)
        (pi-tdd-state-warning s)
      ) ;%copy-state
    ) ;define

    (define (pi-tdd-set-turn-index s index)
      (%copy-state s
        (pi-tdd-state-phase s)
        (pi-tdd-state-task s)
        (pi-tdd-state-test-files s)
        (pi-tdd-state-impl-files s)
        (pi-tdd-state-last-test-run s)
        (pi-tdd-state-phase-history s)
        index
        (pi-tdd-state-warning s)
      ) ;%copy-state
    ) ;define

    (define (pi-tdd-stale-test-run? s)
      (let ((run (pi-tdd-state-last-test-run s)))
        (and run (< (cdr (assoc 'turn-index run)) (pi-tdd-state-turn-index s)))
      ) ;let
    ) ;define

    (define (%advance-to s phase warning)
      (values
        (%copy-state s
          phase
          (pi-tdd-state-task s)
          (pi-tdd-state-test-files s)
          (pi-tdd-state-impl-files s)
          (pi-tdd-state-last-test-run s)
          (append (pi-tdd-state-phase-history s) (list (%phase-entry phase)))
          (pi-tdd-state-turn-index s)
          warning
        ) ;%copy-state
        warning
      ) ;values
    ) ;define

    (define (%stay s warning)
      (values (pi-tdd-set-warning s warning) warning)
    ) ;define

    (define (%advance-from-red s trigger)
      (cond ((eq? trigger 'tests-fail) (%advance-to s 'green #f))
            ((eq? trigger 'manual-advance) (%advance-to s 'green #f))
            ((eq? trigger 'tests-error)
             (%stay s
               "Fix the test error (typo? missing import?). Don't write implementation yet."
             ) ;%stay
            ) ;
            ((eq? trigger 'tests-pass)
             (%stay s
               "Tests should be failing. You're testing existing behavior, not new behavior. Fix the test to assert the new, unimplemented behavior."
             ) ;%stay
            ) ;
            (else (values s #f))
      ) ;cond
    ) ;define

    (define (%advance-from-green s trigger)
      (cond ((eq? trigger 'tests-pass) (%advance-to s 'refactor #f))
            ((eq? trigger 'manual-advance) (%advance-to s 'refactor #f))
            ((eq? trigger 'tests-fail)
             (%stay s "Tests still failing. Continue implementing.")
            ) ;
            ((eq? trigger 'tests-error)
             (%stay s "Runtime error in implementation. Fix it.")
            ) ;
            (else (values s #f))
      ) ;cond
    ) ;define

    (define (%advance-from-refactor s trigger)
      (cond ((eq? trigger 'tests-pass) (%advance-to s 'complete #f))
            ((eq? trigger 'manual-advance) (%advance-to s 'complete #f))
            ((eq? trigger 'tests-fail)
             (%stay s "Refactoring broke tests. Revert the last change.")
            ) ;
            ((eq? trigger 'tests-error)
             (%stay s "Runtime error after refactoring. Revert the last change.")
            ) ;
            (else (values s #f))
      ) ;cond
    ) ;define

    (define (%advance-from-complete s trigger)
      (cond ((eq? trigger 'manual-advance)
             (%stay s
               "TDD cycle is already complete. Start a new task with --red-green to begin a new cycle."
             ) ;%stay
            ) ;
            ((eq? trigger 'tests-pass) (values s #f))
            ((eq? trigger 'tests-fail)
             (%stay s "TDD cycle is complete. Start a new task with --red-green.")
            ) ;
            (else (values s #f))
      ) ;cond
    ) ;define

    (define (pi-tdd-advance-phase state trigger)
      (let ((phase (pi-tdd-state-phase state)))
        (cond ((eq? trigger 'reset) (values (pi-tdd-idle-state) #f))
              ((eq? phase 'idle)
               (values state "No active TDD session. Use --red-green to start one.")
              ) ;
              ((eq? phase 'red) (%advance-from-red state trigger))
              ((eq? phase 'green) (%advance-from-green state trigger))
              ((eq? phase 'refactor) (%advance-from-refactor state trigger))
              ((eq? phase 'complete) (%advance-from-complete state trigger))
              (else (values state #f))
        ) ;cond
      ) ;let
    ) ;define

    ;; Process-local state holder: script mode never persists TDD state to disk.
    (define %tdd-state-holder (vector #f))

    (define (pi-tdd-current-state)
      (vector-ref %tdd-state-holder 0)
    ) ;define

    (define (pi-tdd-set-current-state! state)
      (vector-set! %tdd-state-holder 0 state)
    ) ;define

    (define (pi-tdd-active?)
      (let ((s (pi-tdd-current-state)))
        (and s (not (eq? (pi-tdd-state-phase s) 'idle)))
      ) ;let
    ) ;define

    (define (pi-tdd-start! task)
      (pi-tdd-set-current-state! (pi-tdd-initial-state task))
    ) ;define

    (define (pi-tdd-reset!)
      (pi-tdd-set-current-state! (pi-tdd-idle-state))
    ) ;define

    (define (pi-tdd-bump-turn!)
      (let ((s (pi-tdd-current-state)))
        (when
          (and s (not (eq? (pi-tdd-state-phase s) 'idle)))
          (pi-tdd-set-current-state! (pi-tdd-set-turn-index s (+ 1 (pi-tdd-state-turn-index s)))
          ) ;pi-tdd-set-current-state!
        ) ;when
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
