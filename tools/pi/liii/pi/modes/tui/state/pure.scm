;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi modes tui state pure)
  (import (scheme base)
    (liii error)
    (liii pi modes tui state types)
    (liii pi modes tui slash-menu)
  ) ;import
  (export state-with-status state-with-messages state-with-input-editor
    state-with-active-tool state-with-scroll-offset state-with-dirty
    state-with-dimensions state-with-slash-menu state-with-stream-finished
  ) ;export
  (begin

    (define (state-with-status st new-status)
      (%validate-state st "state-with-status")
      (%validate-status new-status "state-with-status")
      (%make-pi-tui-state new-status
        (state-messages st)
        (state-input-editor st)
        (state-active-tool st)
        (state-scroll-offset st)
        #t
        (state-cols st)
        (state-rows st)
        (state-slash-menu st)
      ) ;%make-pi-tui-state
    ) ;define

    (define (state-with-messages st new-messages)
      (%validate-state st "state-with-messages")
      (%validate-messages new-messages "state-with-messages")
      (%make-pi-tui-state (state-status st)
        new-messages
        (state-input-editor st)
        (state-active-tool st)
        (state-scroll-offset st)
        #t
        (state-cols st)
        (state-rows st)
        (state-slash-menu st)
      ) ;%make-pi-tui-state
    ) ;define

    (define (state-with-input-editor st new-editor)
      (%validate-state st "state-with-input-editor")
      (%make-pi-tui-state (state-status st)
        (state-messages st)
        new-editor
        (state-active-tool st)
        (state-scroll-offset st)
        #t
        (state-cols st)
        (state-rows st)
        (state-slash-menu st)
      ) ;%make-pi-tui-state
    ) ;define

    (define (state-with-active-tool st new-tool)
      (%validate-state st "state-with-active-tool")
      (%make-pi-tui-state (state-status st)
        (state-messages st)
        (state-input-editor st)
        new-tool
        (state-scroll-offset st)
        #t
        (state-cols st)
        (state-rows st)
        (state-slash-menu st)
      ) ;%make-pi-tui-state
    ) ;define

    (define (state-with-scroll-offset st new-offset)
      (%validate-state st "state-with-scroll-offset")
      (%validate-scroll-offset new-offset "state-with-scroll-offset")
      (%make-pi-tui-state (state-status st)
        (state-messages st)
        (state-input-editor st)
        (state-active-tool st)
        new-offset
        #t
        (state-cols st)
        (state-rows st)
        (state-slash-menu st)
      ) ;%make-pi-tui-state
    ) ;define

    (define (state-with-dirty st dirty?)
      (%validate-state st "state-with-dirty")
      (%validate-dirty dirty? "state-with-dirty")
      (%make-pi-tui-state (state-status st)
        (state-messages st)
        (state-input-editor st)
        (state-active-tool st)
        (state-scroll-offset st)
        dirty?
        (state-cols st)
        (state-rows st)
        (state-slash-menu st)
      ) ;%make-pi-tui-state
    ) ;define

    (define (state-with-dimensions st new-cols new-rows)
      (%validate-state st "state-with-dimensions")
      (%validate-dimension new-cols "cols" "state-with-dimensions")
      (%validate-dimension new-rows "rows" "state-with-dimensions")
      (%make-pi-tui-state (state-status st)
        (state-messages st)
        (state-input-editor st)
        (state-active-tool st)
        (state-scroll-offset st)
        #t
        new-cols
        new-rows
        (state-slash-menu st)
      ) ;%make-pi-tui-state
    ) ;define

    (define (state-with-slash-menu st new-menu)
      (%validate-state st "state-with-slash-menu")
      (unless (pi-tui-slash-menu? new-menu)
        (type-error "state-with-slash-menu: expected <pi-tui-slash-menu>")
      ) ;unless
      (%make-pi-tui-state (state-status st)
        (state-messages st)
        (state-input-editor st)
        (state-active-tool st)
        (state-scroll-offset st)
        #t
        (state-cols st)
        (state-rows st)
        new-menu
      ) ;%make-pi-tui-state
    ) ;define

    (define (state-with-stream-finished st . maybe-target-status)
      (%validate-state st "state-with-stream-finished")
      (let ((target (if (pair? maybe-target-status) (car maybe-target-status) 'idle)))
        (%validate-status target "state-with-stream-finished")
        (state-with-status st target)
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
