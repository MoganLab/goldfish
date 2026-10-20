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
    (liii pi modes tui editor)
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
    state-input-text
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
    state-submit-prompt!
    ;; Editor exports
    make-pi-tui-editor
    pi-tui-editor?
    pi-tui-editor-text
    pi-tui-editor-set-text!
    pi-tui-editor-clear!
    pi-tui-editor-lines
    pi-tui-editor-cursor
    pi-tui-editor-set-cursor!
    pi-tui-editor-prompt
    pi-tui-editor-set-prompt!
    pi-tui-editor-continuation-prompt
    pi-tui-editor-set-continuation-prompt!
    pi-tui-editor-placeholder
    pi-tui-editor-set-placeholder!
    pi-tui-editor-focused?
    pi-tui-editor-set-focused!
    pi-tui-editor-history
    pi-tui-editor-push-history!
    pi-tui-editor-handle-key
    pi-tui-editor-render
    pi-tui-editor-render-boxed
    pi-tui-editor-on-submit
    pi-tui-editor-set-on-submit!
    pi-tui-editor-on-change
    pi-tui-editor-set-on-change!
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
