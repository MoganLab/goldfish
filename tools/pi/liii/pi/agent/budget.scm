;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi agent budget)
  (import (scheme base) (liii string) (liii list))
  (export pi-format-turn-budget-status)
  (begin

    (define (%format-warning-lines max-turns turn deadline-sec remaining-sec
              session-timeout
            ) ;%format-warning-lines
      (let* ((low-turns? (and (number? max-turns) (> max-turns 0) (<= (- max-turns turn) 3)))
             (low-time?
               (and deadline-sec (< remaining-sec (max 60 (* (or session-timeout 600) 0.1))))
             ) ;low-time?
            ) ;
        (cond ((and low-turns? low-time?)
               '("<budget_warning>"
                 "CRITICAL BUDGET WARNING: You are nearly out of turns AND time! Stop issuing new tool operations. Provide final conclusions and summary immediately."
                 "</budget_warning>")
              ) ;
              (low-turns? '("<budget_warning>"
                            "WARNING: Approaching maximum turn limit (<= 3 turns left). Wrap up immediately and present final output."
                            "</budget_warning>")
              ) ;low-turns?
              (low-time? '("<budget_warning>"
                           "WARNING: Approaching session timeout deadline (< 60s or < 10% time left). Finalize work and report results."
                           "</budget_warning>")
              ) ;low-time?
              (else '())
        ) ;cond
      ) ;let*
    ) ;define

    (define (pi-format-turn-budget-status turn max-turns remaining-sec
              deadline-sec session-timeout
            ) ;pi-format-turn-budget-status
      (let* ((turn-info
               (if (and (number? max-turns) (> max-turns 0))
                 (let ((rem-turns (- max-turns turn)))
                   (string-append "- Turn Status: Turn "
                     (number->string turn)
                     " (out of max "
                     (number->string max-turns)
                     ", "
                     (number->string rem-turns)
                     " remaining)."
                   ) ;string-append
                 ) ;let
                 (string-append "- Turn Status: Turn "
                   (number->string turn)
                   " (unlimited turns)."
                 ) ;string-append
               ) ;if
             ) ;turn-info
             (time-info
               (if deadline-sec
                 (let ((rem (max 0 (round remaining-sec))))
                   (string-append "- Time Status: "
                     (number->string (inexact->exact rem))
                     " seconds remaining before session timeout."
                   ) ;string-append
                 ) ;let
                 "- Time Status: Unlimited (no session timeout)."
               ) ;if
             ) ;time-info
             (warnings (%format-warning-lines max-turns turn deadline-sec
                         remaining-sec session-timeout
                       ) ;%format-warning-lines
             ) ;warnings
             (items (append (list "<current_turn_budget>" turn-info time-info "</current_turn_budget>")
                      warnings
                    ) ;append
             ) ;items
            ) ;
        (string-join items "\n")
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
