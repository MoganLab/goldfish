;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi modes tui state types)
  (import (scheme base) (liii error))
  (export <pi-tui-state>
    %make-pi-tui-state
    pi-tui-state?
    state-valid-status?
    ;; Getters
    state-status
    pi-tui-state-status
    %state-set-status-raw!
    state-messages
    pi-tui-state-messages
    %state-set-messages-raw!
    state-input-editor
    pi-tui-state-input-editor
    %state-set-input-editor-raw!
    state-active-tool
    pi-tui-state-active-tool
    %state-set-active-tool-raw!
    state-scroll-offset
    pi-tui-state-scroll-offset
    %state-set-scroll-offset-raw!
    state-dirty?
    pi-tui-state-dirty?
    %state-set-dirty-raw!
    state-cols
    pi-tui-state-cols
    %state-set-cols-raw!
    state-rows
    pi-tui-state-rows
    %state-set-rows-raw!
    state-slash-menu
    pi-tui-state-slash-menu
    %state-set-slash-menu-raw!
    ;; Validators
    %validate-status
    %validate-messages
    %validate-scroll-offset
    %validate-dirty
    %validate-dimension
    %validate-state
  ) ;export
  (begin

    (define-record-type <pi-tui-state>
      (%make-pi-tui-state status messages input-editor active-tool scroll-offset
        dirty? cols rows slash-menu
      ) ;%make-pi-tui-state
      pi-tui-state?
      (status state-status %state-set-status-raw!)
      (messages state-messages %state-set-messages-raw!)
      (input-editor state-input-editor %state-set-input-editor-raw!)
      (active-tool state-active-tool %state-set-active-tool-raw!)
      (scroll-offset state-scroll-offset %state-set-scroll-offset-raw!)
      (dirty? state-dirty? %state-set-dirty-raw!)
      (cols state-cols %state-set-cols-raw!)
      (rows state-rows %state-set-rows-raw!)
      (slash-menu state-slash-menu %state-set-slash-menu-raw!)
    ) ;define-record-type

    (define <pi-tui-state> '<pi-tui-state>)

    (define pi-tui-state-status state-status)
    (define pi-tui-state-messages state-messages)
    (define pi-tui-state-input-editor state-input-editor)
    (define pi-tui-state-active-tool state-active-tool)
    (define pi-tui-state-scroll-offset state-scroll-offset)
    (define pi-tui-state-dirty? state-dirty?)
    (define pi-tui-state-cols state-cols)
    (define pi-tui-state-rows state-rows)
    (define pi-tui-state-slash-menu state-slash-menu)

    (define (state-valid-status? s)
      (and (memq s '(idle running streaming confirm exit)) #t)
    ) ;define

    (define (%validate-status s caller)
      (unless (state-valid-status? s)
        (type-error (string-append caller
                      ": invalid status, expected 'idle, 'running, 'streaming, 'confirm, 'exit"
                    ) ;string-append
        ) ;type-error
      ) ;unless
    ) ;define

    (define (%validate-messages msgs caller)
      (unless (list? msgs)
        (type-error (string-append caller ": expected list for messages"))
      ) ;unless
    ) ;define

    (define (%validate-scroll-offset offset caller)
      (unless (and (exact-integer? offset) (>= offset 0))
        (type-error (string-append caller ": expected non-negative integer for scroll-offset")
        ) ;type-error
      ) ;unless
    ) ;define

    (define (%validate-dirty dirty caller)
      (unless (boolean? dirty)
        (type-error (string-append caller ": expected boolean for dirty?"))
      ) ;unless
    ) ;define

    (define (%validate-dimension dim name caller)
      (unless (and (exact-integer? dim) (> dim 0))
        (type-error (string-append caller ": expected positive integer for " name))
      ) ;unless
    ) ;define

    (define (%validate-state st caller)
      (unless (pi-tui-state? st)
        (type-error (string-append caller ": expected <pi-tui-state>"))
      ) ;unless
    ) ;define

  ) ;begin
) ;define-library
