;;
;; Copyright (c) 2025 Mario Zechner
;; Copyright (C) 2024-2026 The Goldfish Scheme Authors
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
    make-game-over-view
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
  ) ;import
  (begin

    (define (%cell-char x y head body food)
      (cond ((equal? (list x y) head) "@")
            ((member (list x y) body) "o")
            ((equal? (list x y) food) "*")
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
      (let* ((score-str (string-append "Score: " (number->string (snake-score game))))
             (len-str
               (string-append "Length: " (number->string (length (snake-body game))))
             ) ;len-str
             (high-str (string-append "Best:  " (number->string (snake-high-score game))))
             (st-str (string-append "Status: " (symbol->string (snake-status game))))
             (spin-str (string-append (spinner-current-frame spinner) " Heartbeat"))
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
          :title         " STATS "
          :style         'single
          :padding-left  1
          :padding-right 1
        ) ;make-box
      ) ;let*
    ) ;define

    (define (make-help-box)
      (let ((content (make-vstack (list (make-text "Arrows / WASD : Move")
                                    (make-text "Space / P      : Pause")
                                    (make-text "Q / Esc        : Quit")
                                    (make-text "Wall wrap     : ON")
                                  ) ;list
                       :spacing 0
                     ) ;make-vstack
            ) ;content
           ) ;
        (make-box content
          :title         " HELP "
          :style         'single
          :padding-left  1
          :padding-right 1
        ) ;make-box
      ) ;let
    ) ;define

    (define (make-game-view game spinner)
      (let* ((grid-lines (render-grid-lines game))
             (board-comp (make-component (lambda (comp width) grid-lines)))
             (board-box (make-box board-comp :title " SNAKE (liii tui) " :style 'double :padding 0)
             ) ;board-box
             (stats-box (make-stats-box game spinner))
             (help-box (make-help-box))
             (side-stack (make-vstack (list stats-box help-box) :spacing 0))
             (board-w (+ (snake-width game) 2))
            ) ;
        (make-hstack (list board-box side-stack)
          :spacing 1
          :widths  (list board-w 'auto)
        ) ;make-hstack
      ) ;let*
    ) ;define

    (define (make-game-over-view game editor-comp)
      (let* ((over-banner (make-text "=== GAME OVER ==="))
             (score-text
               (make-text (string-append "Final Score: " (number->string (snake-score game))))
             ) ;score-text
             (prompt-text (make-text "Register player name:"))
             (content (make-vstack (list over-banner score-text prompt-text editor-comp) :spacing 1)
             ) ;content
            ) ;
        (make-box content
          :title          " RECORD "
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
