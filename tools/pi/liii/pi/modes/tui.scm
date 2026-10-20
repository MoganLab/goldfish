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
    (liii pi modes tui layout)
    (liii pi modes tui message)
    (liii pi modes tui viewport)
    (liii pi modes tui streaming)
    (liii pi modes tui thinking)
    (liii pi modes tui tool-card)
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
    state-cols
    pi-tui-state-cols
    state-rows
    pi-tui-state-rows
    ;; Pure functional updates
    state-with-status
    state-with-messages
    state-with-input-editor
    state-with-active-tool
    state-with-scroll-offset
    state-with-dirty
    state-with-dimensions
    state-with-stream-finished
    ;; In-place mutations
    state-mark-dirty!
    state-clear-dirty!
    state-set-status!
    state-set-messages!
    state-set-input-editor!
    state-set-active-tool!
    state-set-scroll-offset!
    state-set-dimensions!
    state-submit-prompt!
    state-append-message!
    state-finish-stream!
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
    ;; Layout exports
    <tui-rect>
    make-tui-rect
    tui-rect?
    rect-top
    rect-left
    rect-width
    rect-height
    <tui-layout>
    make-tui-layout
    tui-layout?
    layout-cols
    layout-rows
    layout-messages-rect
    layout-editor-rect
    layout-status-rect
    layout-narrow?
    layout-compact?
    compute-editor-height
    compute-tui-layout
    detect-terminal-dimensions
    handle-screen-resize!
    ;; Message & Viewport exports
    <pi-tui-message>
    make-pi-tui-message
    pi-tui-message?
    message-role
    message-content
    normalize-pi-tui-message
    message-role-prefix
    message-role-indent
    render-message-lines
    compute-max-scroll-offset
    compute-viewport-window
    viewport-scroll-indicator
    render-messages-viewport
    compute-committed-lines
    render-messages-viewport-stream
    ;; Streaming exports
    extract-stream-delta
    append-stream-chunk
    state-append-stream-chunk!
    state-with-stream-chunk
    render-tui-screen-lines
    render-tui-screen!
    stream-screen-render!
    ;; Thinking exports
    <pi-tui-thinking-card>
    make-pi-tui-thinking-card
    pi-tui-thinking-card?
    thinking-card-content
    thinking-card-token-count
    thinking-card-collapsed?
    thinking-card-completed?
    thinking-card-scroll-offset
    thinking-card-toggle!
    thinking-card-append-content!
    thinking-card-set-completed!
    thinking-card-set-token-count!
    thinking-card-set-scroll-offset!
    thinking-card-with-toggle
    render-thinking-collapsed
    render-thinking-expanded
    render-thinking-card
    state-find-active-thinking-card
    state-toggle-thinking-card!
    ;; Tool Card exports
    <pi-tui-tool-card>
    make-pi-tui-tool-card
    pi-tui-tool-card?
    tool-card-id
    tool-card-tool-name
    tool-card-args-summary
    tool-card-status
    tool-card-elapsed
    tool-card-result-summary
    tool-card-spinner
    tool-card-start-time
    tool-card-tick!
    tool-card-complete!
    tool-card-fail!
    render-tool-card
    format-tool-args-summary
    format-tool-result-summary
    state-active-tool-card
    state-handle-tool-start!
    state-tick-active-tools!
    state-handle-tool-end!
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
