;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi modes tui loop dispatch)
  (import (scheme base)
    (liii pi modes tui state)
    (liii pi modes tui editor)
    (liii pi modes tui thinking)
    (liii pi modes tui slash-menu)
    (liii pi modes tui confirm)
    (liii pi modes tui loop opts)
  ) ;import
  (export %dispatch-tui-key)
  (begin

    (define (%dispatch-custom-key on-key st key)
      (if (not on-key)
        (values st #f)
        (let ((res (%call-on-key on-key st key)))
          (cond ((pi-tui-state? res) (values res #f))
                ((eq? res 'exit) (state-set-status! st 'exit) (values st #t))
                ((eq? res 'handled) (values st #t))
                (else (values st #f))
          ) ;cond
        ) ;let
      ) ;if
    ) ;define

    (define (%dispatch-confirm-key st key key-handled?)
      (if (or key-handled? (eq? (state-status st) 'exit))
        key-handled?
        (dispatch-confirm-key! st key)
      ) ;if
    ) ;define

    (define (%dispatch-slash-key st key key-handled?)
      (if (or key-handled? (eq? (state-status st) 'exit))
        key-handled?
        (dispatch-slash-menu-key! st key)
      ) ;if
    ) ;define

    (define (%dispatch-editor-key st key key-handled?)
      (if
        (or key-handled?
          (eq? (state-status st) 'exit)
          (not (eq? (state-status st) 'idle))
          (not (pi-tui-editor? (state-input-editor st)))
        ) ;or
        key-handled?
        (let* ((ed (state-input-editor st)) (handled? (pi-tui-editor-handle-key ed key)))
          (when handled?
            (state-sync-slash-menu! st)
            (state-mark-dirty! st)
          ) ;when
          handled?
        ) ;let*
      ) ;if
    ) ;define

    (define (%dispatch-scroll-key st key key-handled?)
      (if (or key-handled? (eq? (state-status st) 'exit))
        key-handled?
        (let ((cur-offset (state-scroll-offset st)))
          (cond ((or (eq? key 'page-up) (eq? key 'shift-up))
                 (state-set-scroll-offset! st (+ cur-offset 5))
                 (state-mark-dirty! st)
                 #t
                ) ;
                ((or (eq? key 'page-down) (eq? key 'shift-down))
                 (state-set-scroll-offset! st (max 0 (- cur-offset 5)))
                 (state-mark-dirty! st)
                 #t
                ) ;
                (else #f)
          ) ;cond
        ) ;let
      ) ;if
    ) ;define

    (define (%dispatch-thinking-key st key key-handled?)
      (if (or key-handled? (eq? (state-status st) 'exit))
        key-handled?
        (cond
         ((or (eq? key 'ctrl-o) (equal? key "\x0f;"))
          (let ((card (state-toggle-thinking-card! st)))
            (and card #t)
          ) ;let
         ) ;
         ((or (eq? key 'tab) (equal? key "\t"))
          (if (string=? (state-input-text st) "")
            (let ((card (state-toggle-thinking-card! st)))
              (and card #t)
            ) ;let
            #f
          ) ;if
         ) ;
         (else #f)
        ) ;cond
      ) ;if
    ) ;define

    (define (%check-exit-key st key key-handled?)
      (unless key-handled?
        (cond ((eq? key 'escape) (state-set-status! st 'exit))
              ((or (equal? key "q") (equal? key "Q")) (state-set-status! st 'exit))
        ) ;cond
      ) ;unless
    ) ;define

    (define (%dispatch-tui-key st key on-key)
      (let*-values (((next-st custom-handled?) (%dispatch-custom-key on-key st key))
                    ((confirm-handled?) (%dispatch-confirm-key next-st key custom-handled?))
                    ((slash-handled?)
                     (%dispatch-slash-key next-st key (or custom-handled? confirm-handled?))
                    ) ;
                    ((thinking-handled?)
                     (%dispatch-thinking-key next-st
                       key
                       (or custom-handled? confirm-handled? slash-handled?)
                     ) ;%dispatch-thinking-key
                    ) ;
                    ((ed-handled?)
                     (%dispatch-editor-key next-st
                       key
                       (or custom-handled? confirm-handled? slash-handled? thinking-handled?)
                     ) ;%dispatch-editor-key
                    ) ;
                    ((scroll-handled?)
                     (%dispatch-scroll-key next-st
                       key
                       (or custom-handled? confirm-handled? slash-handled?
                         thinking-handled? ed-handled?
                       ) ;or
                     ) ;%dispatch-scroll-key
                    ) ;
                   ) ;
        (%check-exit-key next-st
          key
          (or custom-handled? confirm-handled? slash-handled? thinking-handled?
            ed-handled? scroll-handled?
          ) ;or
        ) ;%check-exit-key
        next-st
      ) ;let*-values
    ) ;define

  ) ;begin
) ;define-library
