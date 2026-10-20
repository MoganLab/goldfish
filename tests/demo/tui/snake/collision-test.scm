(import (liii check)
  (demo tui snake logic)
) ;import

(check-set-mode! 'report-failed)

;; 1. 蛇身自碰测试：长度为 5 的蛇回环咬到自身
;; 蛇身呈 U 型：头部在 (5, 5)，身体在 (6, 5), (6, 6), (5, 6), (4, 6)
;; 当前向上，下一步向右转向将撞击自身 (6, 5) 节
(let* ((body '((5 5) (5 6) (6 6) (6 5) (7 5)))
       (g0 (make-snake-game :width 20 :height 12 :snake body :dir 'right :food '(0 0)))
       (g1 (snake-tick g0))
      ) ;
  ;; 头部移动到 (6 5)，而 (6 5) 正好是身子，自碰游戏结束
  (check (snake-status g1) => 'game-over)
) ;let*

;; 2. 退出键导致 game-over 测试
(let* ((g0 (make-snake-game :status 'running))
       (g1 (snake-tick g0 "q"))
      ) ;
  (check (snake-status g1) => 'game-over)
) ;let*

(let* ((g0 (make-snake-game :status 'running))
       (g1 (snake-tick g0 'escape))
      ) ;
  (check (snake-status g1) => 'game-over)
) ;let*

(check-report)
