;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi modes tui editor render)
  (import (scheme base)
    (scheme write)
    (liii base)
    (liii string)
    (liii tui text-width)
    (liii tui box)
    (liii tui component)
    (liii tui editor)
    (liii pi modes tui editor types)
  ) ;import
  (export %pi-tui-editor-render pi-tui-editor-render-boxed)
  (begin

    (define (%pi-tui-editor-render comp width)
      (let* ((data (component-data comp))
             (buf (ed-data-buffer data))
             (lines (buffer-lines buf))
             (row (buffer-row buf))
             (col (buffer-col buf))
             (prompt (ed-data-prompt data))
             (cont-prompt (ed-data-continuation-prompt data))
             (ph (ed-data-placeholder data))
             (focused? (ed-data-focused? data))
             (cursor-style (ed-data-cursor-style data))
             (empty? (and (= (length lines) 1) (string=? (car lines) "")))
            ) ;
        (if (and empty? (not focused?) (not (string=? ph "")))
          (let ((line (string-append prompt ph)))
            (if (<= width 0) (list line) (wrap-text line width))
          ) ;let
          (let loop
            ((rest lines) (r 0) (acc '()))
            (if (null? rest)
              (reverse acc)
              (let* ((prefix (if (= r 0) prompt cont-prompt))
                     (line (car rest))
                     (rendered-body (if (and focused? (= r row))
                                      (render-line-with-cursor line col #t cursor-style)
                                      line
                                    ) ;if
                     ) ;rendered-body
                     (full-line (string-append prefix rendered-body))
                    ) ;
                (loop (cdr rest) (+ r 1) (cons full-line acc))
              ) ;let*
            ) ;if
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    (define (%keyword-value key opts default)
      (let loop
        ((rest opts))
        (cond ((null? rest) default)
              ((and (pair? (cdr rest)) (eq? (car rest) key)) (cadr rest))
              ((null? (cdr rest)) default)
              (else (loop (cddr rest)))
        ) ;cond
      ) ;let
    ) ;define

    (define (pi-tui-editor-render-boxed comp width . opts)
      (let* ((style (%keyword-value :style opts 'rounded))
             (title (%keyword-value :title opts #f))
             (padding (%keyword-value :padding opts 0))
             (bx (make-box comp :style style :title title :padding padding))
            ) ;
        (component-render bx width)
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
