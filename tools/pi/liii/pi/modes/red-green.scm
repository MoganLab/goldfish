;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi modes red-green)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii pi args)
    (liii pi tools)
    (liii pi red-green state)
    (liii pi red-green report)
    (liii pi modes output)
    (liii pi modes flow)
  ) ;import
  (export run-red-green-flow pi-tdd-extend-tool-names)
  (begin

    (define %red-green-tools '("tdd_status" "tdd_advance" "tdd_reset"))

    (define (%replace-field key value cfg)
      (cons (cons key value) (filter (lambda (pair) (not (eq? (car pair) key))) cfg))
    ) ;define

    (define (pi-tdd-extend-tool-names base parsed)
      (if (pi-args-ref parsed 'no-tools)
        base
        (let* ((extras (exclude-tools-from-list %red-green-tools (pi-args-ref parsed 'exclude-tools))
               ) ;extras
               (missing
                 (filter (lambda (name) (not (member name base))) extras)
               ) ;missing
              ) ;
          (append base missing)
        ) ;let*
      ) ;if
    ) ;define

    (define (%red-green-config parsed cfg)
      (%replace-field 'tools
        (pi-tdd-extend-tool-names (cdr (assoc 'tools cfg)) parsed)
        cfg
      ) ;%replace-field
    ) ;define

    (define (%task-message initial-msg)
      (string-append "Implement the following using TDD (red-green-refactor): "
        initial-msg
      ) ;string-append
    ) ;define

    (define (%last-message res-msgs)
      (and (pair? res-msgs) (car (reverse res-msgs)))
    ) ;define

    (define (%summary-text)
      (let ((state (pi-tdd-current-state)))
        (if state (pi-format-tdd-summary state) "")
      ) ;let
    ) ;define

    (define (%print-finish res-msgs)
      (let ((code (handle-print-result (%last-message res-msgs))))
        (display (%summary-text))
        (newline)
        code
      ) ;let
    ) ;define

    (define (%json-finish res-msgs)
      (display-stderr (%summary-text))
      0
    ) ;define

    (define (run-red-green-flow parsed cfg mode initial-msg)
      (when (string-null? (string-trim-both initial-msg))
        (error 'pi-arg-error
          "--red-green requires a task description (message text or @file)"
        ) ;error
      ) ;when
      (pi-tdd-start! (string-trim-both initial-msg))
      (let* ((tdd-cfg (%red-green-config parsed cfg))
             (task-msg (%task-message initial-msg))
            ) ;
        (if (eq? mode 'json)
          (run-session-flow parsed tdd-cfg task-msg emit-json-event %json-finish)
          (run-session-flow parsed tdd-cfg task-msg #f %print-finish)
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
