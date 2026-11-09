;; snake-tick
;; 推进贪吃蛇一帧：处理移动、吃食、转向、暂停与退出
;;
;; 语法
;; ----
;; (snake-tick game . opt-dir)
;;
;; 参数
;; ----
;; game : snake-game?\n当前游戏状态；opt-dir : symbol/string，可选的输入键
;;
;; 返回值
;; ------
;; snake-game?\n推进一帧后的新游戏状态

(import (scheme base)
  (liii check)
  (liii tui snake logic)
) ;import

(check-set-mode! 'report-failed)

;; 1. 正常步进移动测试
(let* ((g0 (make-snake-game :width 20 :height 12 :snake '((5 5) (4 5) (3 5)) :dir 'right))
       (g1 (snake-tick g0))
      ) ;
  (check (snake-head g1) => '(6 5))
  (check (snake-body g1) => '((6 5) (5 5) (4 5)))
  (check (snake-tick-count g1) => 1)
  (check (snake-status g1) => 'running)
) ;let*

;; 2. 边步进边转向测试
(let* ((g0 (make-snake-game :width 20 :height 12 :snake '((5 5) (4 5) (3 5)) :dir 'right))
       (g1 (snake-tick g0 'up))
      ) ;
  (check (snake-dir g1) => 'up)
  (check (snake-head g1) => '(5 4))
) ;let*

;; 3. 穿墙移动测试（右边缘穿越回左侧）
(let* ((g0 (make-snake-game :width 20 :height 12 :snake '((19 5) (18 5) (17 5)) :dir 'right))
       (g1 (snake-tick g0))
      ) ;
  (check (snake-head g1) => '(0 5))
  (check (snake-body g1) => '((0 5) (19 5) (18 5)))
  (check (snake-status g1) => 'running)
) ;let*

;; 4. 暂停与恢复测试
(let* ((g0 (make-snake-game :status 'running))
       (g1 (snake-tick g0 'space))
       (g2 (snake-tick g1))
       (g3 (snake-tick g2 'space))
      ) ;
  (check (snake-status g1) => 'paused)
  (check (snake-status g2) => 'paused)
  (check (snake-status g3) => 'running)
) ;let*

;; 5. 吃到食物：蛇身增长、分数与最高分更新
(let* ((g0 (make-snake-game :width 20 :height 12 :snake '((5 5) (4 5) (3 5))
             :dir 'right :food '(6 5) :score 0 :high-score 0
           ) ;make-snake-game
       ) ;g0
       (g1 (snake-tick g0))
      ) ;
  (check (snake-head g1) => '(6 5))
  (check (length (snake-body g1)) => 4)
  (check (snake-score g1) => 10)
  (check (snake-high-score g1) => 10)
  (check (member (snake-food g1) (snake-body g1)) => #f)
) ;let*

;; 6. 蛇身自碰：头部回环咬到自身，游戏结束
(let* ((body '((5 5) (5 6) (6 6) (6 5) (7 5)))
       (g0 (make-snake-game :width 20 :height 12 :snake body :dir 'right :food '(0 0)))
       (g1 (snake-tick g0))
      ) ;
  (check (snake-status g1) => 'game-over)
) ;let*

;; 7. 退出键导致 game-over
(let* ((g0 (make-snake-game :status 'running))
       (g1 (snake-tick g0 "q"))
      ) ;
  (check (snake-status g1) => 'game-over)
) ;let*

(check-report)
