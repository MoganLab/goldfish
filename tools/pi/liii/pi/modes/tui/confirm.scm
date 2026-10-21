;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi modes tui confirm)
  (import (scheme base)
    (liii error)
    (liii pi modes tui confirm types)
    (liii pi modes tui confirm detect)
    (liii pi modes tui confirm render)
    (liii pi modes tui state)
    (liii pi modes tui editor)
  ) ;import
  (export <pi-tui-confirm-dialog> make-confirm-dialog make-confirm-request
    confirm-dialog? confirm-dialog-id confirm-dialog-tool-call-id
    confirm-dialog-tool-name confirm-dialog-args confirm-dialog-danger-level
    confirm-dialog-target-summary confirm-dialog-diff-preview
    confirm-dialog-decision confirm-dialog-set-decision!
    confirm-dialog-rejected-result tool-dangerous? classify-tool-danger
    extract-target-summary extract-diff-preview
    make-confirm-tool-call-interceptor render-confirm-dialog
    overlay-confirm-dialog-lines dispatch-confirm-key!
  ) ;export
  (begin

    (define (dispatch-confirm-key! st key)
      (if
        (not (and (pi-tui-state? st)
               (eq? (state-status st) 'confirm)
               (confirm-dialog? (state-confirm-dialog st))
             ) ;and
        ) ;not
        #f
        (let ((dlg (state-confirm-dialog st)))
          (cond ((or (equal? key "y") (equal? key "Y"))
                 (confirm-dialog-set-decision! dlg 'allowed)
                 (state-set-confirm-dialog! st #f)
                 (state-set-status! st 'running)
                 (state-mark-dirty! st)
                 #t
                ) ;
                ((or (equal? key "n") (equal? key "N") (eq? key 'escape) (equal? key "\x1b;"))
                 (confirm-dialog-set-decision! dlg 'rejected)
                 (state-set-confirm-dialog! st #f)
                 (state-set-status! st 'idle)
                 (state-mark-dirty! st)
                 #t
                ) ;
                ((or (equal? key "e") (equal? key "E"))
                 (confirm-dialog-set-decision! dlg 'edit)
                 (let* ((ed (state-input-editor st))
                        (tool (confirm-dialog-tool-name dlg))
                        (args (confirm-dialog-args dlg))
                        (text-to-edit (cond ((string=? tool "bash") (extract-diff-preview tool args))
                                            (else (confirm-dialog-target-summary dlg))
                                      ) ;cond
                        ) ;text-to-edit
                       ) ;
                   (when (pi-tui-editor? ed)
                     (pi-tui-editor-set-text! ed text-to-edit)
                   ) ;when
                 ) ;let*
                 (state-set-confirm-dialog! st #f)
                 (state-set-status! st 'idle)
                 (state-mark-dirty! st)
                 #t
                ) ;
                (else
                  ;; Consume any other key while in confirm mode
                  #t
                ) ;else
          ) ;cond
        ) ;let
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
