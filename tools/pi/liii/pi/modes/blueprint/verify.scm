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

(define-library (liii pi modes blueprint verify)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii pi args)
    (liii pi modes blueprint common)
    (liii pi blueprint types)
    (liii pi blueprint transition)
    (liii pi blueprint gate)
    (liii pi blueprint runtime)
  ) ;import
  (export run-blueprint-verify-flow)
  (begin

    (define (%gate-line gate result)
      (let ((passed (cdr (assoc 'passed result)))
            (duration (cdr (assoc 'duration-ms result)))
           ) ;
        (string-append "- ["
          (if passed "PASS" "FAIL")
          "] "
          (pi-gate-description gate)
          (if (and (number? duration) (> duration 0))
            (string-append " (" (number->string duration) "ms)")
            ""
          ) ;if
        ) ;string-append
      ) ;let
    ) ;define

    (define (%approval-result approve?)
      (if approve?
        (list (cons 'passed #t)
          (cons 'error #f)
          (cons 'output "")
          (cons 'duration-ms 0)
        ) ;list
        (list (cons 'passed #f)
          (cons 'error "User approval required")
          (cons 'output "")
          (cons 'duration-ms 0)
        ) ;list
      ) ;if
    ) ;define

    (define (%run-gates parsed cfg bp phase-id gates i timeout)
      (if (= i (length gates))
        (values bp '() #t)
        (let* ((gate (list-ref gates i))
               (type (pi-gate-type gate))
               (result
                 (cond ((string=? type "user_approval")
                        (%approval-result (pi-args-ref parsed 'approve))
                       ) ;
                       ((string=? type "custom_command") (pi-run-blueprint-gate gate timeout))
                       (else #f)
                 ) ;cond
               ) ;result
              ) ;
          (if (not result)
            (values bp (list (string-append "- SKIP: unsupported gate type " type)) #f)
            (let* ((passed (cdr (assoc 'passed result)))
                   (err (cdr (assoc 'error result)))
                   (bp1 (pi-blueprint-set-gate bp phase-id i passed (and (not passed) err)))
                  ) ;
              (pi-blueprint-append-event! (if passed "verification_passed" "verification_failed")
                #f
                (string-append "Gate "
                  (number->string i)
                  " "
                  (pi-gate-description gate)
                  (if (and (not passed) (string? err)) (string-append ": " err) "")
                ) ;string-append
              ) ;pi-blueprint-append-event!
              (let-values (((bp2 lines all-passed)
                            (%run-gates parsed cfg bp1 phase-id gates (+ i 1) timeout)
                           ) ;
                          ) ;
                (values bp2 (cons (%gate-line gate result) lines) (and all-passed passed))
              ) ;let-values
            ) ;let*
          ) ;if
        ) ;let*
      ) ;if
    ) ;define

    (define (%finish-verify mode bp phase-id lines all-passed)
      (let* ((bp1 (if all-passed (pi-blueprint-advance-phase bp) bp))
             (phase
               (find (lambda (p) (string=? (pi-phase-id p) phase-id))
                 (pi-blueprint-phases bp1)
               ) ;find
             ) ;phase
            ) ;
        (pi-blueprint-updated-with-history! bp1
          (if all-passed "phase_verified" "verification_failed")
          #f
          (if all-passed
            (string-append "Phase " phase-id " verified")
            (string-append "Phase " phase-id " verification failed")
          ) ;if
        ) ;pi-blueprint-updated-with-history!
        (pi-bp-mode-out mode (string-join lines "\n"))
        (if all-passed
          (begin
            (pi-bp-mode-out mode (string-append "Phase " phase-id " verified."))
            (when (string=? (pi-blueprint-status bp1) "completed")
              (pi-bp-mode-out mode "Blueprint complete.")
            ) ;when
            0
          ) ;begin
          (begin
            (when
              (and phase
                (pair?
                  (filter (lambda (g) (not (pi-gate-passed? g))) (pi-phase-gates phase))
                ) ;pair?
              ) ;and
              (when
                (pair?
                  (filter (lambda (g) (string=? (pi-gate-type g) "user_approval"))
                    (pi-phase-gates phase)
                  ) ;filter
                ) ;pair?
                (pi-bp-mode-out mode
                  "User approval still required. Re-run with --approve to approve."
                ) ;pi-bp-mode-out
              ) ;when
              (pi-bp-mode-out mode
                "Phase not advanced: some verification gates did not pass."
              ) ;pi-bp-mode-out
            ) ;when
            1
          ) ;begin
        ) ;if
      ) ;let*
    ) ;define

    (define (run-blueprint-verify-flow parsed cfg mode)
      (let* ((bp (pi-blueprint-load! (pi-bp-mode-state-path parsed)))
             (phase-id (and bp (pi-blueprint-active-phase-id bp)))
             (phase
               (and phase-id
                 (find (lambda (p) (string=? (pi-phase-id p) phase-id)) (pi-blueprint-phases bp))
               ) ;and
             ) ;phase
            ) ;
        (cond ((not bp) (begin (pi-bp-mode-out mode "No active blueprint.") 0))
              ((not phase) (begin (pi-bp-mode-out mode "No active phase to verify.") 0))
              (else
                (let ((timeout (pi-bp-mode-timeout parsed cfg)))
                  (let-values (((bp1 lines all-passed)
                                (%run-gates parsed cfg bp phase-id (pi-phase-gates phase) 0 timeout)
                               ) ;
                              ) ;
                    (%finish-verify mode bp1 phase-id lines all-passed)
                  ) ;let-values
                ) ;let
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
