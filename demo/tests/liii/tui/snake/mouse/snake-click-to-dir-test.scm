;; snake-click->dir
;; 根据点击屏幕坐标相对蛇头的主轴偏移决定转向方向
;;
;; 语法
;; ----
;; (snake-click->dir game click-x click-y origin-x origin-y)
;;
;; 参数
;; ----
;; game : snake-game?\n当前游戏状态；click-x, click-y : integer?\n点击的屏幕坐标
;;
;; origin-x, origin-y : integer?\n棋盘左上角屏幕坐标（含边框）
;;
;; 返回值
;; ------
;; symbol? 或 #f\nup/down/left/right；点击点与蛇头重合时返回 #f

(import (scheme base)
  (liii check)
  (liii tui snake logic)
  (liii tui snake mouse)
) ;import

(check-set-mode! 'report-failed)

;; 蛇头在棋盘格 (5,3)，棋盘原点 (2,1)，即蛇头屏幕坐标 (8,5)
(define (make-test-game dir head-x head-y)
  (make-snake-game :width 20 :height 10
    :snake (list (list head-x head-y) (list (- head-x 1) head-y))
    :dir dir
    :food (list 15 8)
  ) ;make-snake-game
) ;define

;; 1. 四象限方向判定
(let ((game (make-test-game 'right 5 3)))
  (check (snake-click->dir game 12 5 2 1) => 'right)
  (check (snake-click->dir game 3 5 2 1) => 'left)
  (check (snake-click->dir game 8 0 2 1) => 'up)
  (check (snake-click->dir game 8 9 2 1) => 'down)
) ;let

;; 2. 主轴判定：水平偏移 >= 垂直偏移时取水平方向
(let ((game (make-test-game 'right 5 3)))
  (check (snake-click->dir game 15 7 2 1) => 'right)
  (check (snake-click->dir game 1 8 2 1) => 'left)
  (check (snake-click->dir game 8 4 2 1) => 'up)
) ;let

;; 3. 点击点与蛇头重合 -> #f
(let ((game (make-test-game 'right 5 3)))
  (check (snake-click->dir game 8 5 2 1) => #f)
) ;let

(check-report)
