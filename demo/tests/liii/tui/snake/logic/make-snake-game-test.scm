;; make-snake-game
;; 创建贪吃蛇游戏状态记录，各字段均有默认值
;;
;; 语法
;; ----
;; (make-snake-game [:width width] [:height height] [:snake snake] [:dir dir]
;;   [:food food] [:score score] [:high-score high-score] [:status status]
;;   [:tick-count tick-count])
;;
;; 参数
;; ----
;; width : integer? (可选，默认 20)\n游戏区宽度；height : integer? (可选，默认 12)\n游戏区高度
;;
;; snake : list? (可选，默认 '((5 5) (4 5) (3 5)))\n蛇身坐标列表
;;
;; dir : symbol? (可选，默认 'right)\n前进方向；food : list? (可选，默认 '(10 5))\n食物坐标
;;
;; score : integer? (可选，默认 0)\n当前分数；high-score : integer? (可选，默认 0)\n最高分
;;
;; status : symbol? (可选，默认 'running)\n游戏状态；tick-count : integer? (可选，默认 0)\n帧计数
;;
;; 返回值
;; ------
;; snake-game?\n新创建的游戏状态

(import (scheme base)
  (liii check)
  (liii tui snake logic)
) ;import

(check-set-mode! 'report-failed)

;; 1. 默认参数构造
(let ((g (make-snake-game)))
  (check (snake-width g) => 20)
  (check (snake-height g) => 12)
  (check (snake-body g) => '((5 5) (4 5) (3 5)))
  (check (snake-dir g) => 'right)
  (check (snake-food g) => '(10 5))
  (check (snake-score g) => 0)
  (check (snake-high-score g) => 0)
  (check (snake-status g) => 'running)
  (check (snake-tick-count g) => 0)
) ;let

;; 2. 关键字参数覆盖默认值
(let ((g (make-snake-game :width 30 :height 15 :snake '((1 1)) :dir 'up
             :food '(2 2) :score 7 :high-score 9 :status 'paused :tick-count 4
           ) ;make-snake-game
      ) ;let*
  ) ;
  (check (snake-width g) => 30)
  (check (snake-height g) => 15)
  (check (snake-body g) => '((1 1)))
  (check (snake-dir g) => 'up)
  (check (snake-food g) => '(2 2))
  (check (snake-score g) => 7)
  (check (snake-high-score g) => 9)
  (check (snake-status g) => 'paused)
  (check (snake-tick-count g) => 4)
) ;let

;; 3. 返回值为 <snake-game> 记录
(check-true (snake-game? (make-snake-game)))

(check-report)
