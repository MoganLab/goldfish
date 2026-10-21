;;
;; Copyright (c) 2025 Mario Zechner
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;

(define-library (demo tui snake view)
  (export render-grid-lines make-stats-box make-help-box make-game-view
    make-game-over-view snake-compute-layout snake-compute-dimensions
  ) ;export
  (import (scheme base)
    (liii base)
    (liii error)
    (liii tui component)
    (liii tui text-width)
    (liii tui box)
    (liii tui loader)
    (liii tui editor)
    (demo tui snake logic)
    (demo tui snake style)
  ) ;import
  (begin

    (define (%cell-char x y head body food)
      (cond ((equal? (list x y) head) (style-head "@"))
            ((member (list x y) body) (style-body "o"))
            ((equal? (list x y) food) (style-food "*"))
            (else " ")
      ) ;cond
    ) ;define

    (define (render-grid-lines game)
      (let* ((w (snake-width game))
             (h (snake-height game))
             (head (snake-head game))
             (body (snake-body game))
             (food (snake-food game))
            ) ;
        (let row-loop
          ((y 0) (rows '()))
          (if (< y h)
            (let col-loop
              ((x 0) (chars '()))
              (if (< x w)
                (col-loop (+ x 1) (cons (%cell-char x y head (cdr body) food) chars))
                (row-loop (+ y 1) (cons (apply string-append (reverse chars)) rows))
              ) ;if
            ) ;let
            (reverse rows)
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

    (define (make-stats-box game spinner)
      (let* ((score-str
               (string-append "Score: " (style-accent (number->string (snake-score game))))
             ) ;score-str
             (len-str
               (string-append "Length: "
                 (style-accent (number->string (length (snake-body game))))
               ) ;string-append
             ) ;len-str
             (high-str
               (string-append "Best:  "
                 (style-accent (number->string (snake-high-score game)))
               ) ;string-append
             ) ;high-str
             (st-str
               (string-append "Status: " (style-accent (symbol->string (snake-status game))))
             ) ;st-str
             (spin-str (string-append (style-heartbeat (spinner-current-frame spinner)) " Heartbeat")
             ) ;spin-str
             (content (make-vstack (list (make-text score-str)
                                     (make-text len-str)
                                     (make-text high-str)
                                     (make-text st-str)
                                     (make-text spin-str)
                                   ) ;list
                        :spacing 0
                      ) ;make-vstack
             ) ;content
            ) ;
        (make-box content
          :title         (style-title " STATS ")
          :style         'single
          :padding-left  1
          :padding-right 1
        ) ;make-box
      ) ;let*
    ) ;define

    (define (make-help-box)
      (let ((content
              (make-vstack
                (list (make-text (string-append (style-dim "Arrows / WASD : ") "Move"))
                  (make-text (string-append (style-dim "Space / P      : ") "Pause"))
                  (make-text (string-append (style-dim "Q / Esc        : ") "Quit"))
                  (make-text (string-append (style-dim "Wall wrap     : ") (style-accent "ON")))
                ) ;list
                :spacing 0
              ) ;make-vstack
            ) ;content
           ) ;
        (make-box content
          :title         (style-dim " HELP ")
          :style         'single
          :padding-left  1
          :padding-right 1
        ) ;make-box
      ) ;let
    ) ;define

    (define (snake-compute-layout term-w term-h)
      (unless (and (integer? term-w) (> term-w 0))
        (type-error "snake-compute-layout: expected positive integer term-width")
      ) ;unless
      (unless (and (integer? term-h) (> term-h 0))
        (type-error "snake-compute-layout: expected positive integer term-height")
      ) ;unless
      (cond
       ((and (>= term-w 54) (>= term-h 12))
        (let ((bw (max 10 (min 50 (- term-w 27)))) (bh (max 5 (min 25 (- term-h 2)))))
          (list (cons 'board-width bw)
            (cons 'board-height bh)
            (cons 'layout 'horizontal)
            (cons 'show-help? #t)
          ) ;list
        ) ;let
       ) ;
       ((and (>= term-w 38) (>= term-h 10))
        (let ((bw (max 10 (min 50 (- term-w 21)))) (bh (max 5 (min 25 (- term-h 2)))))
          (list (cons 'board-width bw)
            (cons 'board-height bh)
            (cons 'layout 'horizontal)
            (cons 'show-help? #f)
          ) ;list
        ) ;let
       ) ;
       (else
         (let ((bw (max 8 (- term-w 2)))
               (bh (if (>= term-h 14) (max 5 (- term-h 9)) (max 4 (- term-h 4))))
              ) ;
           (list (cons 'board-width bw)
             (cons 'board-height bh)
             (cons 'layout 'vertical)
             (cons 'show-help? #f)
           ) ;list
         ) ;let
       ) ;else
      ) ;cond
    ) ;define

    (define (snake-compute-dimensions term-w term-h)
      (let ((info (snake-compute-layout term-w term-h)))
        (list (cdr (assq 'board-width info)) (cdr (assq 'board-height info)))
      ) ;let
    ) ;define

    (define* (make-game-view game spinner (term-width 60) (term-height 24))
      (let* ((layout-info (snake-compute-layout term-width term-height))
             (layout (cdr (assq 'layout layout-info)))
             (show-help? (cdr (assq 'show-help? layout-info)))
             (grid-lines (render-grid-lines game))
             (board-comp (make-component (lambda (comp width) grid-lines)))
             (board-box (make-box board-comp
                          :title   (style-title " SNAKE (liii tui) ")
                          :style   'double
                          :padding 0
                        ) ;make-box
             ) ;board-box
             (stats-box (make-stats-box game spinner))
             (board-w (+ (snake-width game) 2))
            ) ;
        (cond
         ((and (eq? layout 'horizontal) show-help?)
          (let* ((help-box (make-help-box))
                 (side-stack (make-vstack (list stats-box help-box) :spacing 0))
                ) ;
            (make-hstack (list board-box side-stack)
              :spacing 1
              :widths  (list board-w 'auto)
            ) ;make-hstack
          ) ;let*
         ) ;
         ((eq? layout 'horizontal)
          (make-hstack (list board-box stats-box) :spacing 1 :widths (list board-w 'auto))
         ) ;
         (else (make-vstack (list board-box stats-box) :spacing 0))
        ) ;cond
      ) ;let*
    ) ;define*

    (define (make-game-over-view game editor-comp)
      (let* ((over-banner (make-text (style-game-over "=== GAME OVER ===")))
             (score-text
               (make-text
                 (string-append "Final Score: "
                   (style-accent (number->string (snake-score game)))
                 ) ;string-append
               ) ;make-text
             ) ;score-text
             (prompt-text (make-text (style-dim "Register player name:")))
             (content (make-vstack (list over-banner score-text prompt-text editor-comp) :spacing 1)
             ) ;content
            ) ;
        (make-box content
          :title          (style-game-over " RECORD ")
          :style          'double
          :padding-left   2
          :padding-right  2
          :padding-top    1
          :padding-bottom 1
        ) ;make-box
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
