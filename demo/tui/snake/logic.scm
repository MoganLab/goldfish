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

(define-library (demo tui snake logic)
  (export <snake-game> make-snake-game snake-game? snake-width snake-height
    snake-body snake-dir snake-food snake-score snake-high-score snake-status
    snake-tick-count snake-head snake-pass-wall snake-normalize-key
    snake-dir-opposite? snake-turn snake-tick snake-spawn-food
  ) ;export
  (import (scheme base) (liii base) (liii error))
  (begin

    (define-record-type <snake-game>
      (%make-snake-game width height body dir food score high-score status tick-count)
      snake-game?
      (width snake-width)
      (height snake-height)
      (body snake-body)
      (dir snake-dir)
      (food snake-food)
      (score snake-score)
      (high-score snake-high-score)
      (status snake-status)
      (tick-count snake-tick-count)
    ) ;define-record-type

    (define <snake-game> '<snake-game>)

    (define (snake-head game)
      (car (snake-body game))
    ) ;define

    (define (snake-pass-wall x y width height)
      (list (modulo x width) (modulo y height))
    ) ;define

    (define (snake-dir-opposite? d1 d2)
      (or (and (eq? d1 'up) (eq? d2 'down))
        (and (eq? d1 'down) (eq? d2 'up))
        (and (eq? d1 'left) (eq? d2 'right))
        (and (eq? d1 'right) (eq? d2 'left))
      ) ;or
    ) ;define

    (define (snake-normalize-key k)
      (cond ((or (eq? k 'up) (equal? k "w") (equal? k "W")) 'up)
            ((or (eq? k 'down) (equal? k "s") (equal? k "S")) 'down)
            ((or (eq? k 'left) (equal? k "a") (equal? k "A")) 'left)
            ((or (eq? k 'right) (equal? k "d") (equal? k "D")) 'right)
            ((or (eq? k 'space) (equal? k " ") (equal? k "p") (equal? k "P")) 'pause)
            ((or (eq? k 'escape) (eq? k 'ctrl-c) (equal? k "q") (equal? k "Q")) 'quit)
            (else #f)
      ) ;cond
    ) ;define

    (define (%drop-last lst)
      (if (or (null? lst) (null? (cdr lst)))
        '()
        (cons (car lst) (%drop-last (cdr lst)))
      ) ;if
    ) ;define

    (define (%dir-delta dir)
      (cond ((eq? dir 'up) '(0 -1))
            ((eq? dir 'down) '(0 1))
            ((eq? dir 'left) '(-1 0))
            ((eq? dir 'right) '(1 0))
            (else '(0 0))
      ) ;cond
    ) ;define

    (define (snake-spawn-food width height body)
      (let loop
        ((attempts 0))
        (let ((fx (modulo (+ (* attempts 7) 3) width))
              (fy (modulo (+ (* attempts 5) 2) height))
             ) ;
          (if (or (member (list fx fy) body) (>= attempts 100))
            (if (>= attempts 100)
              (let search
                ((x 0) (y 0))
                (cond ((>= y height) '(0 0))
                      ((>= x width) (search 0 (+ y 1)))
                      ((not (member (list x y) body)) (list x y))
                      (else (search (+ x 1) y))
                ) ;cond
              ) ;let
              (loop (+ attempts 1))
            ) ;if
            (list fx fy)
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (snake-turn game key-or-dir)
      (let ((cmd (snake-normalize-key key-or-dir)))
        (if
          (and cmd
            (memq cmd '(up down left right))
            (not (snake-dir-opposite? (snake-dir game) cmd))
          ) ;and
          (%make-snake-game (snake-width game)
            (snake-height game)
            (snake-body game)
            cmd
            (snake-food game)
            (snake-score game)
            (snake-high-score game)
            (snake-status game)
            (snake-tick-count game)
          ) ;%make-snake-game
          game
        ) ;if
      ) ;let
    ) ;define

    (define (snake-tick game . args)
      (let* ((input (if (null? args) #f (car args)))
             (cmd (if input (snake-normalize-key input) #f))
            ) ;
        (cond ((eq? cmd 'quit)
               (%make-snake-game (snake-width game)
                 (snake-height game)
                 (snake-body game)
                 (snake-dir game)
                 (snake-food game)
                 (snake-score game)
                 (snake-high-score game)
                 'game-over
                 (snake-tick-count game)
               ) ;%make-snake-game
              ) ;
              ((eq? cmd 'pause)
               (let ((new-st (if (eq? (snake-status game) 'running) 'paused 'running)))
                 (%make-snake-game (snake-width game)
                   (snake-height game)
                   (snake-body game)
                   (snake-dir game)
                   (snake-food game)
                   (snake-score game)
                   (snake-high-score game)
                   new-st
                   (snake-tick-count game)
                 ) ;%make-snake-game
               ) ;let
              ) ;
              ((not (eq? (snake-status game) 'running)) game)
              (else
                (let* ((turned-game (if cmd (snake-turn game cmd) game))
                       (cur-dir (snake-dir turned-game))
                       (delta (%dir-delta cur-dir))
                       (head (snake-head turned-game))
                       (w (snake-width turned-game))
                       (h (snake-height turned-game))
                       (new-head (snake-pass-wall (+ (car head) (car delta)) (+ (cadr head) (cadr delta)) w h)
                       ) ;new-head
                       (ate? (equal? new-head (snake-food turned-game)))
                       (old-body (snake-body turned-game))
                       (check-body (if ate? old-body (%drop-last old-body)))
                      ) ;
                  (if (member new-head check-body)
                    ;; 自碰 -> game-over
                    (%make-snake-game w
                      h
                      old-body
                      cur-dir
                      (snake-food turned-game)
                      (snake-score turned-game)
                      (snake-high-score turned-game)
                      'game-over
                      (+ (snake-tick-count turned-game) 1)
                    ) ;%make-snake-game
                    (let* ((new-body (cons new-head (if ate? old-body (%drop-last old-body))))
                           (new-score (if ate? (+ (snake-score turned-game) 10) (snake-score turned-game)))
                           (new-high (max new-score (snake-high-score turned-game)))
                           (new-food (if ate? (snake-spawn-food w h new-body) (snake-food turned-game)))
                          ) ;
                      (%make-snake-game w
                        h
                        new-body
                        cur-dir
                        new-food
                        new-score
                        new-high
                        'running
                        (+ (snake-tick-count turned-game) 1)
                      ) ;%make-snake-game
                    ) ;let*
                  ) ;if
                ) ;let*
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    (define*
      (make-snake-game (width 20)
        (height 12)
        (snake '((5 5) (4 5) (3 5)))
        (dir 'right)
        (food '(10 5))
        (score 0)
        (high-score 0)
        (status 'running)
        (tick-count 0)
      ) ;make-snake-game
      (%make-snake-game width height snake dir food score high-score status
        tick-count
      ) ;%make-snake-game
    ) ;define*

  ) ;begin
) ;define-library
