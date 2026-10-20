(import (liii check)
  (liii string)
  (liii tui component)
  (liii tui text-width)
  (liii tui keys)
  (liii tui terminal)
  (liii tui box)
  (liii tui loader)
  (liii tui editor)
  (liii tui main-screen)
  (demo tui snake logic)
  (demo tui snake view)
) ;import

(check-set-mode! 'report-failed)

;; =========================================================================
;; 任务 063: demo/tui/ 终端贪吃蛇交互游戏与 (liii tui) 全能力脱机验收脚本
;; =========================================================================

;; 1. (liii tui component & text-width & keys) 核心能力覆盖
(let ((t (make-text "Goldfish Snake (liii tui)")))
  (check (component? t) => #t)
  (check (display-width "Goldfish Snake (liii tui)") => 25)
  (check (parse-key "\x1b;[A") => 'up)
  (check (parse-key "\x1b;[B") => 'down)
) ;let

;; 2. (liii tui box & vstack & hstack & loader) 界面排版能力覆盖
(let* ((spinner (make-spinner))
       (game (make-snake-game :width 15 :height 8 :score 20))
       (view (make-game-view game spinner))
       (lines (component-render view 55))
      ) ;
  (check (component? view) => #t)
  (check (pair? lines) => #t)
  ;; 验证转轮推进
  (let ((f0 (spinner-current-frame spinner)))
    (spinner-tick! spinner)
    (let ((f1 (spinner-current-frame spinner)))
      (check (string? f0) => #t)
      (check (string? f1) => #t)
    ) ;let
  ) ;let
) ;let*

;; 3. (liii tui terminal & poll) 虚拟终端模拟按键注入与弹出回放
(let ((vt (make-virtual-terminal 50 20)))
  (virtual-terminal-push-key! vt 'up)
  (virtual-terminal-push-key! vt 'left)
  (virtual-terminal-push-key! vt "w")
  (check (terminal-poll-key vt) => 'up)
  (check (terminal-poll-key vt) => 'left)
  (check (terminal-poll-key vt) => "w")
  (check (terminal-poll-key vt) => #f)
) ;let

;; 4. 贪吃蛇四面穿墙（Wrap-around）全量断言（10 边界覆盖）
(check (snake-pass-wall 20 5 20 12) => '(0 5))
(check (snake-pass-wall -1 5 20 12) => '(19 5))
(check (snake-pass-wall 8 12 20 12) => '(8 0))
(check (snake-pass-wall 8 -1 20 12) => '(8 11))
(check (snake-pass-wall -1 -1 20 12) => '(19 11))
(check (snake-pass-wall 20 12 20 12) => '(0 0))
(check (snake-pass-wall -2 4 20 12) => '(18 4))
(check (snake-pass-wall 21 4 20 12) => '(1 4))
(check (snake-pass-wall 4 -3 20 12) => '(4 9))
(check (snake-pass-wall 4 15 20 12) => '(4 3))

;; 5. 贪吃蛇吃食物、身长增加、得分增加与最高分更新
(let* ((g0 (make-snake-game
             :width      20
             :height     12
             :snake      '((5 5) (4 5) (3 5))
             :dir        'right
             :food       '(6 5)
             :score      0
             :high-score 10
           ) ;make-snake-game
       ) ;g0
       (g1 (snake-tick g0))
      ) ;
  (check (snake-head g1) => '(6 5))
  (check (length (snake-body g1)) => 4)
  (check (snake-score g1) => 10)
  (check (snake-high-score g1) => 10)
) ;let*

;; 6. 蛇身自碰（Collision）判定与游戏结束状态
(let* ((body '((5 5) (5 6) (6 6) (6 5) (7 5)))
       (g0 (make-snake-game :width 20 :height 12 :snake body :dir 'right :food '(0
                                                                                 0)))
       (g1 (snake-tick g0))
      ) ;
  (check (snake-status g1) => 'game-over)
) ;let*

;; 7. (liii tui editor) 游戏结束后玩家登记与按键交互
(let ((ed (make-editor :placeholder "Player Name")))
  (component-handle-input ed "S")
  (component-handle-input ed "a")
  (component-handle-input ed "m")
  (check (editor-text ed) => "Sam")
  (component-handle-input ed 'backspace)
  (check (editor-text ed) => "Sa")
) ;let

;; 8. (liii tui main-screen) 差分渲染驱动到 virtual-terminal
(let* ((vt (make-virtual-terminal 65 22))
       (screen (make-main-screen vt))
       (spinner (make-spinner))
       (game (make-snake-game :width 18 :height 8 :food '(8 4)))
       (view (make-game-view game spinner))
       (lines (component-render view 60))
      ) ;
  ;; 初次渲染
  (screen-render! screen lines)
  (let ((scr0 (virtual-terminal-screen vt)))
    (check (pair? scr0) => #t)
    (check (string-contains? (car scr0) "SNAKE") => #t)
  ) ;let
  ;; 推进一帧后差分渲染
  (let* ((game2 (snake-tick game))
         (view2 (make-game-view game2 spinner))
         (lines2 (component-render view2 60))
        ) ;
    (screen-render! screen lines2)
    (let ((scr1 (virtual-terminal-screen vt)))
      (check (pair? scr1) => #t)
    ) ;let
  ) ;let*
) ;let*

(check-report)
