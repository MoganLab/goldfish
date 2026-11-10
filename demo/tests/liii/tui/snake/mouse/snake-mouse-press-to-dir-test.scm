;; snake-mouse-press->dir
;; 处理鼠标事件：只响应左键 press，换算点击方向
;;
;; 语法
;; ----
;; (snake-mouse-press->dir game event origin-x origin-y)
;;
;; 参数
;; ----
;; game : snake-game?\n当前游戏状态；event : mouse-event?\n鼠标事件
;;
;; origin-x, origin-y : integer?\n棋盘左上角屏幕坐标（含边框）
;;
;; 返回值
;; ------
;; symbol? 或 #f\nup/down/left/right；非左键 press 事件返回 #f

(import (scheme base)
  (liii check)
  (liii tui mouse)
  (liii tui snake logic)
  (liii tui snake mouse)
) ;import

(check-set-mode! 'report-failed)

(define (make-test-game dir head-x head-y)
  (make-snake-game :width 20 :height 10
    :snake (list (list head-x head-y) (list (- head-x 1) head-y))
    :dir dir
    :food (list 15 8)
  ) ;make-snake-game
) ;define

;; 1. press 事件 + 绝对坐标：蛇头屏幕 (8,5)，点击 (11,5) -> 向右
(let ((game (make-test-game 'right 5 3)))
  (check (snake-mouse-press->dir game
           (make-mouse-event 'press 'left 11 5)
           2 1)
    => 'right)
) ;let

;; 2. release/wheel 事件被忽略 -> #f
(let ((game (make-test-game 'right 5 3)))
  (check (snake-mouse-press->dir game
           (make-mouse-event 'release 'left 11 5)
           2 1)
    => #f)
  (check (snake-mouse-press->dir game
           (make-mouse-event 'wheel-up 'none 11 5)
           2 1)
    => #f)
) ;let

(check-report)
