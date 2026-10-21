;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi modes tui confirm render)
  (import (scheme base)
    (liii error)
    (liii string)
    (liii tui component)
    (liii tui text-width)
    (liii tui box)
    (liii pi modes tui confirm types)
  ) ;import
  (export render-confirm-dialog overlay-confirm-dialog-lines)
  (begin

    (define (%truncate-line str max-w)
      (if (<= (display-width str) max-w) str (truncate-to-width str max-w))
    ) ;define

    (define (render-confirm-dialog dlg width height)
      (%validate-confirm-dialog dlg "render-confirm-dialog")
      (unless (and (exact-integer? width) (> width 0))
        (type-error "render-confirm-dialog: expected positive integer width")
      ) ;unless
      (unless (and (exact-integer? height) (> height 0))
        (type-error "render-confirm-dialog: expected positive integer height")
      ) ;unless
      (let* ((box-w (max 1 (min width 64)))
             (inner-w (max 1 (- box-w 2)))
             (tool-line (%truncate-line (string-append "Tool  : " (confirm-dialog-tool-name dlg))
                          inner-w
                        ) ;%truncate-line
             ) ;tool-line
             (target-line (%truncate-line (string-append "Target: " (confirm-dialog-target-summary dlg))
                            inner-w
                          ) ;%truncate-line
             ) ;target-line
             (danger-line
               (%truncate-line (string-append "Danger: " (symbol->string (confirm-dialog-danger-level dlg)))
                 inner-w
               ) ;%truncate-line
             ) ;danger-line
             (prompt-line (%truncate-line "[y] 允许 / [n] 阻止 / [e] 编辑" inner-w))
             (raw-diff (confirm-dialog-diff-preview dlg))
             (diff-lines (if (string=? raw-diff "") '() (string-split raw-diff "\n")))
             (max-preview-lines (max 0 (min 3 (- height 7))))
             (preview-lines
               (if (or (null? diff-lines) (<= max-preview-lines 0))
                 '()
                 (let loop
                   ((rest diff-lines) (i 0) (acc '()))
                   (if (or (null? rest) (>= i max-preview-lines))
                     (cons (%truncate-line "Preview:" inner-w) (reverse acc))
                     (loop (cdr rest)
                       (+ i 1)
                       (cons (%truncate-line (string-append "  " (car rest)) inner-w) acc)
                     ) ;loop
                   ) ;if
                 ) ;let
               ) ;if
             ) ;preview-lines
             (all-items (append (list tool-line target-line danger-line)
                          preview-lines
                          (list "" prompt-line)
                        ) ;append
             ) ;all-items
             (content (apply make-vstack all-items))
             (box (make-box content :style 'double :title "CONFIRM OPERATION"))
             (lines (component-render box box-w))
            ) ;
        (if (> (length lines) height)
          (let loop
            ((rest lines) (i 0) (acc '()))
            (if (or (null? rest) (>= i height))
              (reverse acc)
              (loop (cdr rest) (+ i 1) (cons (car rest) acc))
            ) ;if
          ) ;let
          lines
        ) ;if
      ) ;let*
    ) ;define

    (define (%normalize-bg-lines bg-lines width height)
      (let loop
        ((rest bg-lines) (i 0) (acc '()))
        (if (>= i height)
          (reverse acc)
          (if (null? rest)
            (loop '() (+ i 1) (cons (make-string width #\space) acc))
            (let* ((ln (car rest)) (w (display-width ln)))
              (loop (cdr rest)
                (+ i 1)
                (cons
                  (if (< w width)
                    (string-append ln (make-string (- width w) #\space))
                    (truncate-to-width ln width)
                  ) ;if
                  acc
                ) ;cons
              ) ;loop
            ) ;let*
          ) ;if
        ) ;if
      ) ;let
    ) ;define

    (define (%merge-dialog-row bg-ln d-ln left-pad right-pad dlg-w)
      (let* ((left-bg (if (> left-pad 0) (truncate-to-width bg-ln left-pad) ""))
             (right-bg
               (if (<= right-pad 0)
                 ""
                 (let ((skip (+ left-pad dlg-w)))
                   (if (<= skip (string-length bg-ln))
                     (substring bg-ln skip (min (string-length bg-ln) (+ skip right-pad)))
                     (make-string right-pad #\space)
                   ) ;if
                 ) ;let
               ) ;if
             ) ;right-bg
            ) ;
        (string-append left-bg d-ln right-bg)
      ) ;let*
    ) ;define

    (define (overlay-confirm-dialog-lines dlg screen-lines width height)
      (%validate-confirm-dialog dlg "overlay-confirm-dialog-lines")
      (unless (and (exact-integer? width) (> width 0))
        (type-error "overlay-confirm-dialog-lines: expected positive integer width")
      ) ;unless
      (unless (and (exact-integer? height) (> height 0))
        (type-error "overlay-confirm-dialog-lines: expected positive integer height")
      ) ;unless
      (let* ((bg-lines (if (list? screen-lines) screen-lines '()))
             (normalized-bg (%normalize-bg-lines bg-lines width height))
             (dlg-lines (render-confirm-dialog dlg width height))
             (dlg-h (length dlg-lines))
             (dlg-w (if (pair? dlg-lines) (display-width (car dlg-lines)) 0))
             (top-pad
               (max 0 (floor (/ (- height dlg-h) 2)))
             ) ;top-pad
             (left-pad
               (max 0 (floor (/ (- width dlg-w) 2)))
             ) ;left-pad
             (right-pad (max 0 (- width left-pad dlg-w)))
            ) ;
        (let loop
          ((r 0) (rest-bg normalized-bg) (acc '()))
          (if (>= r height)
            (reverse acc)
            (let* ((bg-ln (if (pair? rest-bg) (car rest-bg) (make-string width #\space)))
                   (next-bg (if (pair? rest-bg) (cdr rest-bg) '()))
                   (dlg-idx (- r top-pad))
                  ) ;
              (if (and (>= dlg-idx 0) (< dlg-idx dlg-h))
                (let* ((d-ln (list-ref dlg-lines dlg-idx))
                       (merged (%merge-dialog-row bg-ln d-ln left-pad right-pad dlg-w))
                      ) ;
                  (loop (+ r 1) next-bg (cons merged acc))
                ) ;let*
                (loop (+ r 1) next-bg (cons bg-ln acc))
              ) ;if
            ) ;let*
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
