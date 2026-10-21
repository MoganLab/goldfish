;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi modes tui confirm types)
  (import (scheme base) (liii error))
  (export <pi-tui-confirm-dialog> make-confirm-dialog make-confirm-request
    confirm-dialog? confirm-dialog-id confirm-dialog-tool-call-id
    confirm-dialog-tool-name confirm-dialog-args confirm-dialog-danger-level
    confirm-dialog-target-summary confirm-dialog-diff-preview
    confirm-dialog-decision confirm-dialog-set-decision!
    confirm-dialog-rejected-result %validate-confirm-dialog
    %validate-danger-level
  ) ;export
  (begin

    (define-record-type <pi-tui-confirm-dialog>
      (%make-confirm-dialog id tool-name args danger-level target-summary
        diff-preview decision
      ) ;%make-confirm-dialog
      confirm-dialog?
      (id confirm-dialog-id)
      (tool-name confirm-dialog-tool-name)
      (args confirm-dialog-args)
      (danger-level confirm-dialog-danger-level)
      (target-summary confirm-dialog-target-summary)
      (diff-preview confirm-dialog-diff-preview)
      (decision confirm-dialog-decision confirm-dialog-set-decision!)
    ) ;define-record-type

    (define <pi-tui-confirm-dialog> '<pi-tui-confirm-dialog>)
    (define confirm-dialog-tool-call-id confirm-dialog-id)

    (define (%valid-danger-level? lvl)
      (and (memq lvl '(low medium high critical)) #t)
    ) ;define

    (define (%validate-danger-level lvl caller)
      (unless (%valid-danger-level? lvl)
        (type-error (string-append caller
                      ": invalid danger level, expected 'low, 'medium, 'high, or 'critical"
                    ) ;string-append
        ) ;type-error
      ) ;unless
    ) ;define

    (define (%validate-confirm-dialog dlg caller)
      (unless (confirm-dialog? dlg)
        (type-error (string-append caller ": expected <pi-tui-confirm-dialog>"))
      ) ;unless
    ) ;define

    (define (make-confirm-dialog . opts)
      (let loop
        ((rest opts)
         (id "")
         (tool-name "")
         (args '())
         (danger-level 'high)
         (target-summary "")
         (diff-preview "")
         (decision 'pending)
        ) ;
        (cond ((null? rest)
               (unless (string? id)
                 (type-error "make-confirm-dialog: expected string for :id")
               ) ;unless
               (unless (string? tool-name)
                 (type-error "make-confirm-dialog: expected string for :tool-name")
               ) ;unless
               (unless (list? args)
                 (type-error "make-confirm-dialog: expected list for :args")
               ) ;unless
               (%validate-danger-level danger-level "make-confirm-dialog")
               (unless (string? target-summary)
                 (type-error "make-confirm-dialog: expected string for :target-summary")
               ) ;unless
               (unless (string? diff-preview)
                 (type-error "make-confirm-dialog: expected string for :diff-preview")
               ) ;unless
               (%make-confirm-dialog id tool-name args danger-level
                 target-summary diff-preview decision
               ) ;%make-confirm-dialog
              ) ;
              ((eq? (car rest) :id)
               (loop (cddr rest)
                 (cadr rest)
                 tool-name
                 args
                 danger-level
                 target-summary
                 diff-preview
                 decision
               ) ;loop
              ) ;
              ((eq? (car rest) :tool-name)
               (loop (cddr rest)
                 id
                 (cadr rest)
                 args
                 danger-level
                 target-summary
                 diff-preview
                 decision
               ) ;loop
              ) ;
              ((eq? (car rest) :args)
               (loop (cddr rest)
                 id
                 tool-name
                 (cadr rest)
                 danger-level
                 target-summary
                 diff-preview
                 decision
               ) ;loop
              ) ;
              ((eq? (car rest) :danger-level)
               (loop (cddr rest)
                 id
                 tool-name
                 args
                 (cadr rest)
                 target-summary
                 diff-preview
                 decision
               ) ;loop
              ) ;
              ((eq? (car rest) :target-summary)
               (loop (cddr rest)
                 id
                 tool-name
                 args
                 danger-level
                 (cadr rest)
                 diff-preview
                 decision
               ) ;loop
              ) ;
              ((eq? (car rest) :diff-preview)
               (loop (cddr rest)
                 id
                 tool-name
                 args
                 danger-level
                 target-summary
                 (cadr rest)
                 decision
               ) ;loop
              ) ;
              ((eq? (car rest) :decision)
               (loop (cddr rest)
                 id
                 tool-name
                 args
                 danger-level
                 target-summary
                 diff-preview
                 (cadr rest)
               ) ;loop
              ) ;
              (else (type-error "make-confirm-dialog: unknown keyword argument"))
        ) ;cond
      ) ;let
    ) ;define

    (define make-confirm-request make-confirm-dialog)

    (define (confirm-dialog-rejected-result dlg)
      (%validate-confirm-dialog dlg "confirm-dialog-rejected-result")
      (list (cons 'isError #t)
        (cons 'content "Operation rejected by user")
        (cons 'result "Operation rejected by user")
      ) ;list
    ) ;define

  ) ;begin
) ;define-library
