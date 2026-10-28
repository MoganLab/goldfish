(import (liii check)
        (liii tui mouse)
        (demo tui snake logic)
        (demo tui snake mouse))

(check-set-mode! 'report-failed)

;; snake-click->dir
;; 根据点击屏幕坐标相对蛇头的主轴偏移，决定转向方向。
;;
;; 语法
;; ----
;; (snake-click->dir game click-x click-y origin-x origin-y)
;;
;; 参数
;; ----
;; game : snake-game
;; click-x/click-y : integer，点击的屏幕坐标（0-based）
;; origin-x/origin-y : integer，棋盘左上角的屏幕坐标（含边框，即棋盘格 (0,0)
;;                    对应屏幕 (origin-x+1, origin-y+1)）
;;
;; 返回值
;; ----
;; 'up/'down/'left/'right 或 #f（点击点与蛇头重合、无法判断方向时）

;; 布局：蛇头在 (5,3)，棋盘原点 (2,1)，即蛇头屏幕坐标 (8,5)
;; （棋盘格 (gx,gy) -> 屏幕 (origin-x+1+gx, origin-y+1+gy)）
(define (make-test-game dir head-x head-y)
  (make-snake-game :width 20 :height 10
    :snake (list (list head-x head-y) (list (- head-x 1) head-y))
    :dir dir
    :food (list 15 8)
  ) ;make-snake-game
) ;define

(let ((game (make-test-game 'right 5 3)))
  ;; 点击蛇头右侧 -> right
  (check (snake-click->dir game 12 5 2 1) => 'right)
  ;; 点击蛇头左侧 -> left（与当前方向相反，由 snake-turn 忽略）
  (check (snake-click->dir game 3 5 2 1) => 'left)
  ;; 点击蛇头上方 -> up
  (check (snake-click->dir game 8 0 2 1) => 'up)
  ;; 点击蛇头下方 -> down
  (check (snake-click->dir game 8 9 2 1) => 'down)
  ;; 主轴判定：水平偏移 > 垂直偏移时取水平方向
  (check (snake-click->dir game 15 7 2 1) => 'right)
  (check (snake-click->dir game 1 8 2 1) => 'left)
  ;; 垂直偏移更大时取垂直方向
  (check (snake-click->dir game 9 0 2 1) => 'up)
  ;; 点击蛇头正上方一格：向上
  (check (snake-click->dir game 8 4 2 1) => 'up)
) ;let

;; 点击点与蛇头重合 -> #f
(let ((game (make-test-game 'right 5 3)))
  (check (snake-click->dir game 8 5 2 1) => #f)
) ;let

;; mouse-event 直接接入：press 事件 + 绝对坐标
(let ((game (make-test-game 'right 5 3)))
  ;; 蛇头屏幕 (8,5)，点击 (11,5) -> 向右
  (check (snake-mouse-press->dir game
           (make-mouse-event 'press 'left 11 5)
           2 1)
    => 'right)
) ;let

;; release/wheel 事件被忽略 -> #f
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

;; 得到的方向交给 snake-turn：反向被忽略、合法则生效
(let ((game (make-test-game 'right 5 3)))
  (check (snake-dir (snake-turn game 'left)) => 'right)  ; 反向忽略
  (check (snake-dir (snake-turn game 'up)) => 'up)       ; 合法转向
) ;let

(check-report)
