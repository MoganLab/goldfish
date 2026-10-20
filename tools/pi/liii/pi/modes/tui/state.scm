;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi modes tui state)
  (import (scheme base)
    (liii error)
    (liii pi modes tui state types)
    (liii pi modes tui editor)
  ) ;import
  (export <pi-tui-state>
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
    ;; Functional pure updates
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
  ) ;export
  (begin

    (define (%parse-state-args args)
      (let loop
        ((rest args)
         (status 'idle)
         (messages '())
         (input-editor "")
         (active-tool #f)
         (scroll-offset 0)
         (dirty? #t)
        ) ;
        (cond ((null? rest)
               (values status messages input-editor active-tool scroll-offset dirty?)
              ) ;
              ((eq? (car rest) :status)
               (loop (cddr rest)
                 (cadr rest)
                 messages
                 input-editor
                 active-tool
                 scroll-offset
                 dirty?
               ) ;loop
              ) ;
              ((eq? (car rest) :messages)
               (loop (cddr rest)
                 status
                 (cadr rest)
                 input-editor
                 active-tool
                 scroll-offset
                 dirty?
               ) ;loop
              ) ;
              ((eq? (car rest) :input-editor)
               (loop (cddr rest) status messages (cadr rest) active-tool scroll-offset dirty?)
              ) ;
              ((eq? (car rest) :active-tool)
               (loop (cddr rest) status messages input-editor (cadr rest) scroll-offset dirty?)
              ) ;
              ((eq? (car rest) :scroll-offset)
               (loop (cddr rest) status messages input-editor active-tool (cadr rest) dirty?)
              ) ;
              ((or (eq? (car rest) :dirty?) (eq? (car rest) :dirty))
               (loop (cddr rest)
                 status
                 messages
                 input-editor
                 active-tool
                 scroll-offset
                 (cadr rest)
               ) ;loop
              ) ;
              (else
                (let ((len (length rest)))
                  (values (if (>= len 1) (list-ref rest 0) status)
                    (if (>= len 2) (list-ref rest 1) messages)
                    (if (>= len 3) (list-ref rest 2) input-editor)
                    (if (>= len 4) (list-ref rest 3) active-tool)
                    (if (>= len 5) (list-ref rest 4) scroll-offset)
                    (if (>= len 6) (list-ref rest 5) dirty?)
                  ) ;values
                ) ;let
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (make-pi-tui-state . args)
      (let-values (((status messages input-editor active-tool scroll-offset dirty?)
                    (%parse-state-args args)
                   ) ;
                  ) ;
        (%validate-status status "make-pi-tui-state")
        (%validate-messages messages "make-pi-tui-state")
        (%validate-scroll-offset scroll-offset "make-pi-tui-state")
        (%validate-dirty dirty? "make-pi-tui-state")
        (%make-pi-tui-state status messages input-editor active-tool
          scroll-offset dirty?
        ) ;%make-pi-tui-state
      ) ;let-values
    ) ;define

    (define (state-with-status st new-status)
      (%validate-state st "state-with-status")
      (%validate-status new-status "state-with-status")
      (%make-pi-tui-state new-status
        (state-messages st)
        (state-input-editor st)
        (state-active-tool st)
        (state-scroll-offset st)
        #t
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
      ) ;%make-pi-tui-state
    ) ;define

    (define (state-mark-dirty! st)
      (%validate-state st "state-mark-dirty!")
      (%state-set-dirty-raw! st #t)
    ) ;define

    (define (state-clear-dirty! st)
      (%validate-state st "state-clear-dirty!")
      (%state-set-dirty-raw! st #f)
    ) ;define

    (define (state-set-status! st new-status)
      (%validate-state st "state-set-status!")
      (%validate-status new-status "state-set-status!")
      (%state-set-status-raw! st new-status)
      (%state-set-dirty-raw! st #t)
    ) ;define

    (define (state-set-messages! st new-messages)
      (%validate-state st "state-set-messages!")
      (%validate-messages new-messages "state-set-messages!")
      (%state-set-messages-raw! st new-messages)
      (%state-set-dirty-raw! st #t)
    ) ;define

    (define (state-set-input-editor! st new-editor)
      (%validate-state st "state-set-input-editor!")
      (%state-set-input-editor-raw! st new-editor)
      (%state-set-dirty-raw! st #t)
    ) ;define

    (define (state-set-active-tool! st new-tool)
      (%validate-state st "state-set-active-tool!")
      (%state-set-active-tool-raw! st new-tool)
      (%state-set-dirty-raw! st #t)
    ) ;define

    (define (state-set-scroll-offset! st new-offset)
      (%validate-state st "state-set-scroll-offset!")
      (%validate-scroll-offset new-offset "state-set-scroll-offset!")
      (%state-set-scroll-offset-raw! st new-offset)
      (%state-set-dirty-raw! st #t)
    ) ;define

    (define (state-input-text st)
      (%validate-state st "state-input-text")
      (let ((ed (state-input-editor st)))
        (cond ((pi-tui-editor? ed) (pi-tui-editor-text ed))
              ((string? ed) ed)
              (else "")
        ) ;cond
      ) ;let
    ) ;define

    (define (state-submit-prompt! st . maybe-target-status)
      (%validate-state st "state-submit-prompt!")
      (let* ((target-status (if (pair? maybe-target-status) (car maybe-target-status) 'running)
             ) ;target-status
             (text (state-input-text st))
             (ed (state-input-editor st))
            ) ;
        (%validate-status target-status "state-submit-prompt!")
        (unless (string=? text "")
          (when (pi-tui-editor? ed)
            (pi-tui-editor-push-history! ed text)
            (pi-tui-editor-clear! ed)
          ) ;when
          (state-set-messages! st (append (state-messages st) (list (cons 'user text))))
          (state-set-status! st target-status)
          (state-mark-dirty! st)
        ) ;unless
        st
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
