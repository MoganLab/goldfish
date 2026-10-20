;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi red-green gate)
  (import (scheme base)
    (liii string)
    (liii pi read)
    (liii pi red-green state)
    (liii pi red-green classify)
  ) ;import
  (export pi-tdd-write-gate pi-tdd-note-edited-file!)
  (begin

    (define (%gated-tool? tool-name)
      (or (equal? tool-name "edit") (equal? tool-name "write"))
    ) ;define

    (define (%argument-path args)
      (let ((path (pi-arg-ref args "path" "")))
        (and (string? path) (not (string-null? (string-trim-both path))) path)
      ) ;let
    ) ;define

    ;; Returns a human-readable rejection message, or #f when the write is allowed.
    (define (pi-tdd-write-gate tool-name args)
      (let ((state (pi-tdd-current-state)))
        (and state
          (eq? (pi-tdd-state-phase state) 'red)
          (%gated-tool? tool-name)
          (let* ((path (%argument-path args))
                 (classification (and path (pi-classify-tdd-file path)))
                ) ;
            (and (eq? classification 'implementation)
              (string-append "TDD RED phase violation: "
                path
                " is an implementation file. Write a failing test first; implementation edits are blocked until the tests fail."
              ) ;string-append
            ) ;and
          ) ;let*
        ) ;and
      ) ;let
    ) ;define

    (define (pi-tdd-note-edited-file! tool-name args is-error?)
      (let ((state (pi-tdd-current-state)))
        (when
          (and state
            (not (eq? (pi-tdd-state-phase state) 'idle))
            (not is-error?)
            (%gated-tool? tool-name)
          ) ;and
          (let* ((path (%argument-path args))
                 (classification (and path (pi-classify-tdd-file path)))
                 (current (pi-tdd-current-state))
                ) ;
            (case classification
             ((test) (pi-tdd-set-current-state! (pi-tdd-add-test-file current path)))
             ((implementation)
              (pi-tdd-set-current-state! (pi-tdd-add-impl-file current path))
             ) ;
             (else #f)
            ) ;case
          ) ;let*
        ) ;when
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
