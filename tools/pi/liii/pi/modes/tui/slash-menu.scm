;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi modes tui slash-menu)
  (import (scheme base)
    (liii error)
    (liii string)
    (liii pi modes tui editor)
    (liii pi modes tui slash-menu types)
    (liii pi modes tui slash-menu render)
  ) ;import
  (export <pi-tui-slash-command> make-pi-tui-slash-command pi-tui-slash-command?
    slash-command-name slash-command-description slash-command-handler
    normalize-slash-command *builtin-slash-commands* collect-slash-commands
    filter-slash-commands <pi-tui-slash-menu> make-pi-tui-slash-menu
    pi-tui-slash-menu? slash-menu-visible? slash-menu-query
    slash-menu-candidates slash-menu-selected-index slash-menu-commands
    slash-menu-show! slash-menu-hide! slash-menu-select-next!
    slash-menu-select-prev! slash-menu-selected-command slash-menu-update-query!
    slash-menu-complete! render-slash-menu overlay-slash-menu-lines
  ) ;export
  (begin

    (define (slash-menu-show! menu)
      (%menu-set-visible! menu #t)
      menu
    ) ;define

    (define (slash-menu-hide! menu)
      (%menu-set-visible! menu #f)
      menu
    ) ;define

    (define (slash-menu-selected-command menu)
      (let ((cands (slash-menu-candidates menu)) (idx (slash-menu-selected-index menu)))
        (if (or (null? cands) (< idx 0) (>= idx (length cands)))
          #f
          (list-ref cands idx)
        ) ;if
      ) ;let
    ) ;define

    (define (slash-menu-select-next! menu)
      (let ((n (length (slash-menu-candidates menu))))
        (if (<= n 0)
          (%menu-set-selected-index! menu 0)
          (let ((next-idx (modulo (+ (slash-menu-selected-index menu) 1) n)))
            (%menu-set-selected-index! menu next-idx)
          ) ;let
        ) ;if
        menu
      ) ;let
    ) ;define

    (define (slash-menu-select-prev! menu)
      (let ((n (length (slash-menu-candidates menu))))
        (if (<= n 0)
          (%menu-set-selected-index! menu 0)
          (let ((prev-idx (modulo (- (+ (slash-menu-selected-index menu) n) 1) n)))
            (%menu-set-selected-index! menu prev-idx)
          ) ;let
        ) ;if
        menu
      ) ;let
    ) ;define

    (define (slash-menu-update-query! menu text)
      (unless (string? text)
        (type-error "slash-menu-update-query!: expected string text")
      ) ;unless
      (cond
       ((or (string=? text "")
          (not (string-starts? text "/"))
          (string-contains text " ")
        ) ;or
        (%menu-set-visible! menu #f)
        (%menu-set-query! menu "")
        #f
       ) ;
       (else
         (let* ((new-cands (filter-slash-commands (slash-menu-commands menu) text))
                (len (length new-cands))
               ) ;
           (%menu-set-visible! menu #t)
           (%menu-set-query! menu text)
           (%menu-set-candidates! menu new-cands)
           (if (<= len 0)
             (%menu-set-selected-index! menu 0)
             (let ((cur-idx (slash-menu-selected-index menu)))
               (if (>= cur-idx len)
                 (%menu-set-selected-index! menu 0)
                 (%menu-set-selected-index! menu cur-idx)
               ) ;if
             ) ;let
           ) ;if
           #t
         ) ;let*
       ) ;else
      ) ;cond
    ) ;define

    (define (slash-menu-complete! menu editor)
      (if (not (slash-menu-visible? menu))
        #f
        (let ((cmd (slash-menu-selected-command menu)))
          (if (not cmd)
            #f
            (let* ((name (slash-command-name cmd)) (completed-text (string-append name " ")))
              (pi-tui-editor-set-text! editor completed-text)
              (pi-tui-editor-set-cursor! editor 0 (string-length completed-text))
              (slash-menu-hide! menu)
              #t
            ) ;let*
          ) ;if
        ) ;let
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
