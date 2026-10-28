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

(import (scheme base)
  (liii base)
  (liii os)
  (liii time)
  (liii tui component)
  (liii tui terminal)
  (liii tui mouse)
  (liii tui loader)
  (liii tui editor)
  (liii tui main-screen)
  (demo tui snake logic)
  (demo tui snake view)
  (demo tui snake mouse)
  (demo tui snake style)
) ;import

(define (%run-game-over-loop screen term game width)
  (let* ((render-w (min width 50))
         (editor-comp (make-editor :placeholder "Your name" :text "Player1"))
         (view (make-game-over-view game editor-comp))
         (lines (component-render view render-w))
        ) ;
    (screen-render! screen lines)
    (let loop
      ((wait-ticks 0))
      (let ((k (terminal-poll-key term)))
        (cond ((or (eq? k 'enter) (eq? k 'escape) (equal? k "q") (>= wait-ticks 80)) #t)
              (k (component-handle-input editor-comp k)
                (let* ((new-view (make-game-over-view game editor-comp))
                       (new-lines (component-render new-view render-w))
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
  (style-enable!)
  (let* ((term (make-standard-terminal))
         (screen (make-main-screen term))
         (spinner (make-spinner))
         (init-sz (terminal-size term))
         (init-cols (car init-sz))
         (init-rows (cadr init-sz))
         (init-dims (snake-compute-dimensions init-cols init-rows))
         (board-w (car init-dims))
         (board-h (cadr init-dims))
         (mid-y (quotient board-h 2))
         (init-snake (list (list 5 mid-y)
                       (list 4 mid-y)
                       (list 3 mid-y)
                       (list 2 mid-y)
                       (list 1 mid-y)
                     ) ;list
         ) ;init-snake
         (init-food (snake-spawn-food board-w board-h init-snake))
         (game (make-snake-game
                 :width  board-w
                 :height board-h
                 :snake  init-snake
                 :dir    'right
                 :food   init-food
               ) ;make-snake-game
         ) ;game
        ) ;
    (dynamic-wind (lambda ()
                    (console-raw-mode! #t)
                    (terminal-hide-cursor! term)
                    ;; 开启鼠标报告：POSIX 终端发 ANSI 序列，
                    ;; Windows 控制台由 C++ 层合成 SGR 字节流
                    (terminal-mouse-mode! term #t)
                    (terminal-flush term)
                    (terminal-clear term)
                  ) ;lambda
      (lambda ()
        (let game-loop
          ((current-game game) (prev-cols init-cols) (prev-rows init-rows))
          (if (eq? (snake-status current-game) 'game-over)
            (begin
              (%run-game-over-loop screen term current-game prev-cols)
              (screen-clear! screen)
              (terminal-show-cursor! term)
              (display "Thanks for playing Snake (liii tui)!\n")
            ) ;begin
            (let* ((cur-sz (terminal-size term))
                   (cur-cols (car cur-sz))
                   (cur-rows (cadr cur-sz))
                   (resized? (or (not (= cur-cols prev-cols)) (not (= cur-rows prev-rows))))
                   (active-game
                     (if resized?
                       (let* ((dims (snake-compute-dimensions cur-cols cur-rows))
                              (nw (car dims))
                              (nh (cadr dims))
                             ) ;
                         (snake-resize current-game nw nh)
                       ) ;let*
                       current-game
                     ) ;if
                   ) ;active-game
                  ) ;
              (when resized?
                (screen-resize! screen cur-cols cur-rows)
              ) ;when
              (spinner-tick! spinner)
              (let* ((view (make-game-view active-game spinner cur-cols cur-rows))
                     (lines (component-render view cur-cols))
                    ) ;
                (screen-render! screen lines)
                (sleep 0.1)
                ;; 排空输入队列：键盘与鼠标（一次 tick 可能积压多个事件）
                (let poll-loop
                  ((key (terminal-poll-key term)))
                  (cond
                    ;; 鼠标点击：转为转向指令，继续排空队列
                    ((mouse-event? key)
                     (let ((dir (snake-mouse-press->dir active-game key 0 0)))
                       (when dir
                         (set! active-game (snake-turn active-game dir))
                       ) ;when
                     ) ;let
                     (poll-loop (terminal-poll-key term))
                    ) ;
                    ;; 退出键
                    ((or (eq? key 'escape) (equal? key "q") (equal? key "Q"))
                     (begin
                       (screen-clear! screen)
                       (terminal-show-cursor! term)
                       (display "Game quit by player.\n")
                     ) ;begin
                    ) ;
                    ;; 无输入：继续游戏循环
                    ((not key) (game-loop (snake-tick active-game #f) cur-cols cur-rows))
                    ;; 其他按键：方向键立即转向（不消耗 tick），
                    ;; 暂停/退出交给 tick；转向后继续排空队列
                    (else
                      (case (snake-normalize-key key)
                            ((up down left right)
                             (set! active-game (snake-turn active-game key))
                             (poll-loop (terminal-poll-key term))
                            ) ;
                            ((pause) (game-loop (snake-tick active-game key) cur-cols cur-rows))
                            (else (poll-loop (terminal-poll-key term)))
                      ) ;case
                    ) ;else
                  ) ;cond
                ) ;let
              ) ;let*
            ) ;let*
          ) ;if
        ) ;let
      ) ;lambda
      (lambda () (terminal-show-cursor! term) (console-raw-mode! #f))
    ) ;dynamic-wind
  ) ;let*
) ;define

(main)
