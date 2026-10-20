;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi modes tui viewport)
  (import (scheme base) (liii base) (liii error) (liii pi modes tui message))
  (export compute-max-scroll-offset compute-viewport-window
    viewport-scroll-indicator render-messages-viewport compute-committed-lines
    render-messages-viewport-stream
  ) ;export
  (begin

    (define (compute-max-scroll-offset total-lines viewport-height)
      (unless (and (exact-integer? total-lines) (>= total-lines 0))
        (type-error "compute-max-scroll-offset: expected non-negative integer total-lines"
        ) ;type-error
      ) ;unless
      (unless (and (exact-integer? viewport-height) (>= viewport-height 0))
        (type-error "compute-max-scroll-offset: expected non-negative integer viewport-height"
        ) ;type-error
      ) ;unless
      (max 0 (- total-lines viewport-height))
    ) ;define

    (define (compute-viewport-window total-lines viewport-height scroll-offset)
      (unless (and (exact-integer? total-lines) (>= total-lines 0))
        (type-error "compute-viewport-window: expected non-negative integer total-lines"
        ) ;type-error
      ) ;unless
      (unless (and (exact-integer? viewport-height) (>= viewport-height 0))
        (type-error "compute-viewport-window: expected non-negative integer viewport-height"
        ) ;type-error
      ) ;unless
      (unless (and (exact-integer? scroll-offset) (>= scroll-offset 0))
        (type-error "compute-viewport-window: expected non-negative integer scroll-offset"
        ) ;type-error
      ) ;unless
      (if (<= total-lines viewport-height)
        (values 0 total-lines)
        (let* ((max-offset (- total-lines viewport-height))
               (clamped-offset (min max-offset scroll-offset))
               (start (- max-offset clamped-offset))
               (end (+ start viewport-height))
              ) ;
          (values start end)
        ) ;let*
      ) ;if
    ) ;define

    (define (viewport-scroll-indicator total-lines viewport-height scroll-offset)
      (unless (and (exact-integer? total-lines) (>= total-lines 0))
        (type-error "viewport-scroll-indicator: expected non-negative integer total-lines"
        ) ;type-error
      ) ;unless
      (unless (and (exact-integer? viewport-height) (>= viewport-height 0))
        (type-error "viewport-scroll-indicator: expected non-negative integer viewport-height"
        ) ;type-error
      ) ;unless
      (unless (and (exact-integer? scroll-offset) (>= scroll-offset 0))
        (type-error "viewport-scroll-indicator: expected non-negative integer scroll-offset"
        ) ;type-error
      ) ;unless
      (if (<= total-lines viewport-height)
        ""
        (let* ((max-offset (- total-lines viewport-height))
               (clamped (min max-offset scroll-offset))
              ) ;
          (cond ((= clamped 0) "")
                ((= clamped max-offset) "[Top]")
                (else
                  (let ((pct (inexact->exact (round (* (/ clamped max-offset) 100)))))
                    (string-append "[" (number->string pct) "%]")
                  ) ;let
                ) ;else
          ) ;cond
        ) ;let*
      ) ;if
    ) ;define

    (define (%sublist lst start end)
      (let loop
        ((rest lst) (i 0) (acc '()))
        (cond ((or (null? rest) (>= i end)) (reverse acc))
              ((>= i start) (loop (cdr rest) (+ i 1) (cons (car rest) acc)))
              (else (loop (cdr rest) (+ i 1) acc))
        ) ;cond
      ) ;let
    ) ;define

    (define (compute-committed-lines messages width)
      (unless (list? messages)
        (type-error "compute-committed-lines: expected list for messages")
      ) ;unless
      (unless (and (exact-integer? width) (> width 0))
        (type-error "compute-committed-lines: expected positive integer width")
      ) ;unless
      (let loop
        ((rest messages) (acc '()))
        (if (null? rest)
          acc
          (let ((lines (render-message-lines (car rest) width)))
            (loop (cdr rest) (append acc lines))
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%render-message-lines-all messages width maybe-committed)
      (let ((committed (and (pair? maybe-committed)
                         (list? (car maybe-committed))
                         (car maybe-committed)
                       ) ;and
            ) ;committed
           ) ;
        (if committed
          (let* ((last-msg (car (reverse messages)))
                 (active-lines (render-message-lines last-msg width))
                ) ;
            (append committed active-lines)
          ) ;let*
          (let loop
            ((rest messages) (acc '()))
            (if (null? rest)
              acc
              (loop (cdr rest) (append acc (render-message-lines (car rest) width)))
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (render-messages-viewport messages width height scroll-offset .
             maybe-committed-lines
            ) ;
      (unless (list? messages)
        (type-error "render-messages-viewport: expected list for messages")
      ) ;unless
      (unless (and (exact-integer? width) (> width 0))
        (type-error "render-messages-viewport: expected positive integer width")
      ) ;unless
      (unless (and (exact-integer? height) (>= height 0))
        (type-error "render-messages-viewport: expected non-negative integer height")
      ) ;unless
      (unless (and (exact-integer? scroll-offset) (>= scroll-offset 0))
        (type-error "render-messages-viewport: expected non-negative integer scroll-offset"
        ) ;type-error
      ) ;unless
      (cond ((zero? height) '())
            ((null? messages) '())
            (else
              (let* ((all-rendered-lines (%render-message-lines-all messages width maybe-committed-lines)
                     ) ;all-rendered-lines
                     (total (length all-rendered-lines))
                    ) ;
                (let-values (((start end) (compute-viewport-window total height scroll-offset)))
                  (%sublist all-rendered-lines start end)
                ) ;let-values
              ) ;let*
            ) ;else
      ) ;cond
    ) ;define

    (define render-messages-viewport-stream render-messages-viewport)

  ) ;begin
) ;define-library
