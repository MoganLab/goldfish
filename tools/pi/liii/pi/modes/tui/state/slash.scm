;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi modes tui state slash)
  (import (scheme base)
    (liii error)
    (liii pi modes tui state types)
    (liii pi modes tui slash-menu)
    (liii pi modes tui editor)
  ) ;import
  (export state-set-slash-menu! state-slash-menu-visible? state-sync-slash-menu!
    state-slash-menu-select-next! state-slash-menu-select-prev!
    state-slash-menu-complete! state-slash-menu-hide! dispatch-slash-menu-key!
  ) ;export
  (begin

    (define (state-set-slash-menu! st menu)
      (%validate-state st "state-set-slash-menu!")
      (unless (pi-tui-slash-menu? menu)
        (type-error "state-set-slash-menu!: expected <pi-tui-slash-menu>")
      ) ;unless
      (%state-set-slash-menu-raw! st menu)
      (%state-set-dirty-raw! st #t)
    ) ;define

    (define (state-slash-menu-visible? st)
      (%validate-state st "state-slash-menu-visible?")
      (let ((menu (state-slash-menu st)))
        (and (pi-tui-slash-menu? menu) (slash-menu-visible? menu))
      ) ;let
    ) ;define

    (define (state-sync-slash-menu! st)
      (%validate-state st "state-sync-slash-menu!")
      (let* ((ed (state-input-editor st))
             (menu (state-slash-menu st))
             (text (cond ((pi-tui-editor? ed) (pi-tui-editor-text ed))
                         ((string? ed) ed)
                         (else "")
                   ) ;cond
             ) ;text
            ) ;
        (when (pi-tui-slash-menu? menu)
          (let ((old-vis (slash-menu-visible? menu))
                (changed? (slash-menu-update-query! menu text))
               ) ;
            (when
              (or changed? (not (eq? old-vis (slash-menu-visible? menu))))
              (%state-set-dirty-raw! st #t)
            ) ;when
          ) ;let
        ) ;when
      ) ;let*
    ) ;define

    (define (state-slash-menu-select-next! st)
      (%validate-state st "state-slash-menu-select-next!")
      (let ((menu (state-slash-menu st)))
        (when (pi-tui-slash-menu? menu)
          (slash-menu-select-next! menu)
          (%state-set-dirty-raw! st #t)
        ) ;when
      ) ;let
    ) ;define

    (define (state-slash-menu-select-prev! st)
      (%validate-state st "state-slash-menu-select-prev!")
      (let ((menu (state-slash-menu st)))
        (when (pi-tui-slash-menu? menu)
          (slash-menu-select-prev! menu)
          (%state-set-dirty-raw! st #t)
        ) ;when
      ) ;let
    ) ;define

    (define (state-slash-menu-complete! st)
      (%validate-state st "state-slash-menu-complete!")
      (let ((menu (state-slash-menu st)) (ed (state-input-editor st)))
        (if (and (pi-tui-slash-menu? menu) (pi-tui-editor? ed))
          (let ((res (slash-menu-complete! menu ed)))
            (when res
              (%state-set-dirty-raw! st #t)
            ) ;when
            res
          ) ;let
          #f
        ) ;if
      ) ;let
    ) ;define

    (define (state-slash-menu-hide! st)
      (%validate-state st "state-slash-menu-hide!")
      (let ((menu (state-slash-menu st)))
        (when (pi-tui-slash-menu? menu)
          (slash-menu-hide! menu)
          (%state-set-dirty-raw! st #t)
        ) ;when
      ) ;let
    ) ;define

    (define (dispatch-slash-menu-key! st key)
      (if (not (state-slash-menu-visible? st))
        #f
        (cond ((eq? key 'up) (state-slash-menu-select-prev! st) #t)
              ((eq? key 'down) (state-slash-menu-select-next! st) #t)
              ((or (eq? key 'tab)
                 (equal? key "\t")
                 (eq? key 'enter)
                 (equal? key "\r")
                 (equal? key "\n")
               ) ;or
               (state-slash-menu-complete! st)
               #t
              ) ;
              ((eq? key 'escape) (state-slash-menu-hide! st) #t)
              (else #f)
        ) ;cond
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
