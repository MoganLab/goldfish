;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi red-green observe)
  (import (scheme base)
    (liii string)
    (liii pi read)
    (liii pi red-green state)
    (liii pi red-green gate)
    (liii pi red-green detect)
  ) ;import
  (export pi-tdd-observe-tool!)
  (begin

    (define (%record-test-run! run)
      (let* ((state (pi-tdd-current-state))
             (run-with-turn
               (append run (list (cons 'turn-index (pi-tdd-state-turn-index state))))
             ) ;run-with-turn
             (state-with-run (pi-tdd-set-last-test-run state run-with-turn))
             (trigger (pi-tdd-test-run-trigger run-with-turn))
            ) ;
        (if trigger
          (let-values (((next warning) (pi-tdd-advance-phase state-with-run trigger)))
            (pi-tdd-set-current-state! (pi-tdd-set-warning next warning))
          ) ;let-values
          (pi-tdd-set-current-state! state-with-run)
        ) ;if
      ) ;let*
    ) ;define

    (define (%observe-bash! args text is-error?)
      (let ((command (pi-arg-ref args "command" "")))
        (when (and (string? command) (pi-tdd-test-run-command? command))
          (let* ((exit-code (pi-tdd-exit-code-from-text text is-error?))
                 (run (pi-tdd-parse-test-run-result text exit-code))
                ) ;
            (when run
              (%record-test-run! run)
            ) ;when
          ) ;let*
        ) ;when
      ) ;let
    ) ;define

    (define (pi-tdd-observe-tool! tool-name args text is-error?)
      (let ((state (pi-tdd-current-state)))
        (when
          (and state (not (eq? (pi-tdd-state-phase state) 'idle)))
          (pi-tdd-note-edited-file! tool-name args is-error?)
          (when (equal? tool-name "bash")
            (%observe-bash! args text is-error?)
          ) ;when
        ) ;when
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
