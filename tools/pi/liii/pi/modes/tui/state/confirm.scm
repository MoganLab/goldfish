;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi modes tui state confirm)
  (import (scheme base)
    (liii error)
    (liii pi modes tui state types)
    (liii pi modes tui confirm types)
  ) ;import
  (export state-set-confirm-dialog! state-request-confirm! state-resolve-confirm!)
  (begin

    (define (state-set-confirm-dialog! st dlg)
      (%validate-state st "state-set-confirm-dialog!")
      (when (and dlg (not (confirm-dialog? dlg)))
        (type-error "state-set-confirm-dialog!: expected <pi-tui-confirm-dialog> or #f")
      ) ;when
      (%state-set-confirm-dialog-raw! st dlg)
      (%state-set-dirty-raw! st #t)
      st
    ) ;define

    (define (state-request-confirm! st dlg)
      (%validate-state st "state-request-confirm!")
      (%validate-confirm-dialog dlg "state-request-confirm!")
      (%state-set-confirm-dialog-raw! st dlg)
      (%state-set-status-raw! st 'confirm)
      (%state-set-dirty-raw! st #t)
      st
    ) ;define

    (define (state-resolve-confirm! st decision)
      (%validate-state st "state-resolve-confirm!")
      (unless (memq decision '(allowed rejected edit))
        (type-error "state-resolve-confirm!: expected 'allowed, 'rejected, or 'edit")
      ) ;unless
      (let ((dlg (state-confirm-dialog st)))
        (when dlg
          (confirm-dialog-set-decision! dlg decision)
        ) ;when
        (%state-set-confirm-dialog-raw! st #f)
        (%state-set-status-raw! st (if (eq? decision 'allowed) 'running 'idle))
        (%state-set-dirty-raw! st #t)
        st
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
