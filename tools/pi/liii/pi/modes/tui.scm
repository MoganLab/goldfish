;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi modes tui)
  (import (scheme base)
    (liii error)
    (liii os)
    (liii tui terminal)
    (liii pi modes tui state)
    (liii pi modes tui loop)
  ) ;import
  (export with-pi-tui-terminal
    run-tui-loop
    run-tui-flow
    ;; State type & constructors
    <pi-tui-state>
    make-pi-tui-state
    pi-tui-state?
    state-valid-status?
    ;; Getters
    state-status
    pi-tui-state-status
    state-messages
    pi-tui-state-messages
    state-input-editor
    pi-tui-state-input-editor
    state-active-tool
    pi-tui-state-active-tool
    state-scroll-offset
    pi-tui-state-scroll-offset
    state-dirty?
    pi-tui-state-dirty?
    ;; Pure functional updates
    state-with-status
    state-with-messages
    state-with-input-editor
    state-with-active-tool
    state-with-scroll-offset
    state-with-dirty
    ;; In-place mutations
    state-mark-dirty!
    state-clear-dirty!
    state-set-status!
    state-set-messages!
    state-set-input-editor!
    state-set-active-tool!
    state-set-scroll-offset!
  ) ;export
  (begin

    (define (with-pi-tui-terminal term thunk)
      (unless (terminal? term)
        (type-error "with-pi-tui-terminal: expected terminal")
      ) ;unless
      (unless (procedure? thunk)
        (type-error "with-pi-tui-terminal: expected procedure for thunk")
      ) ;unless
      (dynamic-wind
        (lambda ()
          (terminal-write term "\x1b;[?1049h")
          (terminal-hide-cursor! term)
          (catch #t (lambda () (console-raw-mode! #t)) (lambda _ #f))
          (terminal-clear term)
          (terminal-flush term)
        ) ;lambda
        thunk
        (lambda ()
          (terminal-show-cursor! term)
          (terminal-write term "\x1b;[?1049l")
          (catch #t (lambda () (console-raw-mode! #f)) (lambda _ #f))
          (terminal-flush term)
        ) ;lambda
      ) ;dynamic-wind
    ) ;define

    (define (run-tui-flow parsed cfg mode initial-msg . maybe-term)
      (let ((term
              (if (and (pair? maybe-term) (terminal? (car maybe-term)))
                (car maybe-term)
                (make-standard-terminal)
              ) ;if
            ) ;term
           ) ;
        (with-pi-tui-terminal term (lambda () (run-tui-loop term)))
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
