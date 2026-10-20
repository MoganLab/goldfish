;;
;; Copyright (C) 2024 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

(define-library (liii tui box border)
  (export <border-style> make-border-style border-style? border-style-name
    border-style-tl border-style-tr border-style-bl border-style-br
    border-style-h border-style-v border-style-resolve border-render-top
    border-render-bottom border-render-line
  ) ;export
  (import (liii base) (liii error) (liii string) (liii tui text-width))
  (begin

    (define-record-type <border-style>
      (%make-border-style name tl tr bl br h v)
      border-style?
      (name border-style-name)
      (tl border-style-tl)
      (tr border-style-tr)
      (bl border-style-bl)
      (br border-style-br)
      (h border-style-h)
      (v border-style-v)
    ) ;define-record-type

    (define <border-style> '<border-style>)

    (define (make-border-style . args)
      (cond ((= (length args) 6) (apply %make-border-style 'custom args))
            ((= (length args) 7) (apply %make-border-style args))
            (else (type-error "make-border-style: requires 6 or 7 arguments (name tl tr bl br h v)"
                  ) ;type-error
            ) ;else
      ) ;cond
    ) ;define

    (define *style-single*
      (%make-border-style 'single "┌" "┐" "└" "┘" "─" "│")
    ) ;define

    (define *style-double*
      (%make-border-style 'double "╔" "╗" "╚" "╝" "═" "║")
    ) ;define

    (define *style-rounded*
      (%make-border-style 'rounded "╭" "╮" "╰" "╯" "─" "│")
    ) ;define

    (define *style-ascii* (%make-border-style 'ascii "+" "+" "+" "+" "-" "|"))

    (define *style-none* (%make-border-style 'none "" "" "" "" "" ""))

    (define (border-style-resolve style)
      (cond ((border-style? style) style)
            ((symbol? style)
             (case style
              ((single) *style-single*)
              ((double) *style-double*)
              ((rounded) *style-rounded*)
              ((ascii) *style-ascii*)
              ((none) *style-none*)
              (else (value-error "unknown border style: " style))
             ) ;case
            ) ;
            (else (type-error "border-style-resolve: style must be a symbol or <border-style>")
            ) ;else
      ) ;cond
    ) ;define

    (define (%string-repeat str n)
      (if (<= n 0)
        ""
        (let loop
          ((i 0) (acc '()))
          (if (>= i n) (apply string-append (reverse acc)) (loop (+ i 1) (cons str acc)))
        ) ;let
      ) ;if
    ) ;define

    (define (%make-filler-pad h h-w fill-w)
      (let* ((cnt (if (> h-w 0) (quotient fill-w h-w) 0))
             (filler (%string-repeat h cnt))
             (pad (make-string (- fill-w (* cnt h-w)) #\space))
            ) ;
        (string-append filler pad)
      ) ;let*
    ) ;define

    (define (%render-top-with-title tl tr h h-w inner-w title)
      (let* ((pre-h (if (> h-w 0) h ""))
             (pre-w (display-width pre-h))
             (avail-title (- inner-w pre-w 1 1 pre-w))
            ) ;
        (if (> avail-title 0)
          (let* ((t-fit (truncate-to-width title avail-title "..."))
                 (t-w (display-width t-fit))
                 (rem-w (max 0 (- avail-title t-w)))
                 (fill (%make-filler-pad h h-w rem-w))
                ) ;
            (string-append tl pre-h " " t-fit " " fill pre-h tr)
          ) ;let*
          (string-append tl (%make-filler-pad h h-w inner-w) tr)
        ) ;if
      ) ;let*
    ) ;define

    (define (border-render-top border width . title-arg)
      (let ((title (if (null? title-arg) #f (car title-arg))))
        (cond ((eq? (border-style-name border) 'none) "")
              ((<= width 0) "")
              (else
                (let* ((tl (border-style-tl border))
                       (tr (border-style-tr border))
                       (h (border-style-h border))
                       (tl-w (display-width tl))
                       (tr-w (display-width tr))
                       (h-w (display-width h))
                      ) ;
                  (if (<= width (+ tl-w tr-w))
                    (truncate-to-width (string-append tl tr) width "")
                    (let ((inner-w (- width tl-w tr-w)))
                      (if (and title (string? title) (> (string-length title) 0))
                        (%render-top-with-title tl tr h h-w inner-w title)
                        (string-append tl (%make-filler-pad h h-w inner-w) tr)
                      ) ;if
                    ) ;let
                  ) ;if
                ) ;let*
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (border-render-bottom border width)
      (cond ((eq? (border-style-name border) 'none) "")
            ((<= width 0) "")
            (else
              (let* ((bl (border-style-bl border))
                     (br (border-style-br border))
                     (h (border-style-h border))
                     (bl-w (display-width bl))
                     (br-w (display-width br))
                     (h-w (display-width h))
                    ) ;
                (if (<= width (+ bl-w br-w))
                  (truncate-to-width (string-append bl br) width "")
                  (let ((inner-w (- width bl-w br-w)))
                    (string-append bl (%make-filler-pad h h-w inner-w) br)
                  ) ;let
                ) ;if
              ) ;let*
            ) ;else
      ) ;cond
    ) ;define

    (define (border-render-line border content-line content-width pad-left pad-right)
      (let* ((none? (eq? (border-style-name border) 'none))
             (v (if none? "" (border-style-v border)))
             (vis-w (display-width content-line))
             (fitted-line
               (cond ((> vis-w content-width) (truncate-to-width content-line content-width ""))
                     ((< vis-w content-width)
                      (string-append content-line (make-string (- content-width vis-w) #\space))
                     ) ;
                     (else content-line)
               ) ;cond
             ) ;fitted-line
            ) ;
        (string-append v
          (make-string pad-left #\space)
          fitted-line
          (make-string pad-right #\space)
          v
        ) ;string-append
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
