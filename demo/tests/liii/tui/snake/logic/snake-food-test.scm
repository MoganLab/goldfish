;; snake-food
;; 读取食物坐标
;;
;; 语法
;; ----
;; (snake-food game)
;;
;; 参数
;; ----
;; game : snake-game?\n当前游戏状态
;;
;; 返回值
;; ------
;; list?\n食物坐标 (x y)

(import (scheme base)
  (liii check)
  (liii tui snake logic)
) ;import

(check-set-mode! 'report-failed)

(check (snake-food (make-snake-game)) => '(10 5))
(check (snake-food (make-snake-game :food '(3 7))) => '(3 7))
(check-catch 'type-error (snake-food "x"))

(check-report)
