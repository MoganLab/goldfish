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

(import (scheme base)
  (liii base)
  (liii time)
  (liii tui component)
  (liii tui terminal)
  (liii tui loader)
  (liii tui editor)
  (liii tui main-screen)
  (demo tui snake logic)
  (demo tui snake view)
) ;import

(define (%run-game-over-loop screen term game)
  (let* ((editor-comp (make-editor :placeholder "Your name" :text "Player1"))
         (view (make-game-over-view game editor-comp))
         (lines (component-render view 45))
        ) ;
    (screen-render! screen lines)
    (let loop
      ((wait-ticks 0))
      (let ((k (terminal-poll-key term)))
        (cond ((or (eq? k 'enter) (eq? k 'escape) (equal? k "q") (>= wait-ticks 80)) #t)
              (k (component-handle-input editor-comp k)
                (let* ((new-view (make-game-over-view game editor-comp))
                       (new-lines (component-render new-view 45))
                      ) ;
                  (screen-render! screen new-lines)
                  (sleep 0.08)
                  (loop (+ wait-ticks 1))
                ) ;let*
              ) ;k
              (else (sleep 0.08) (loop (+ wait-ticks 1)))
        ) ;cond
      ) ;let
    ) ;let
  ) ;let*
) ;define

(define (main)
  (let* ((term (make-standard-terminal))
         (screen (make-main-screen term))
         (spinner (make-spinner))
         (init-food '(10 5))
         (game (make-snake-game
                 :width  20
                 :height 10
                 :snake  '((5 5) (4 5) (3 5) (2 5) (1 5))
                 :dir    'right
                 :food   init-food
               ) ;make-snake-game
         ) ;game
        ) ;
    (terminal-hide-cursor! term)
    (terminal-clear term)
    (let game-loop
      ((current-game game))
      (if (eq? (snake-status current-game) 'game-over)
        (begin
          (%run-game-over-loop screen term current-game)
          (screen-clear! screen)
          (terminal-show-cursor! term)
          (display "Thanks for playing Snake (liii tui)!\n")
        ) ;begin
        (begin
          (spinner-tick! spinner)
          (let* ((view (make-game-view current-game spinner))
                 (lines (component-render view 60))
                ) ;
            (screen-render! screen lines)
            (sleep 0.1)
            (let ((key (terminal-poll-key term)))
              (if (or (eq? key 'escape) (equal? key "q") (equal? key "Q"))
                (begin
                  (screen-clear! screen)
                  (terminal-show-cursor! term)
                  (display "Game quit by player.\n")
                ) ;begin
                (game-loop (snake-tick current-game key))
              ) ;if
            ) ;let
          ) ;let*
        ) ;begin
      ) ;if
    ) ;let
  ) ;let*
) ;define

(main)
