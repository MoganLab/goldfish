;; snake-width
;; 读取游戏区域的宽度
;;
;; 语法
;; ----
;; (snake-width game)
;;
;; 参数
;; ----
;; game : snake-game?\n当前游戏状态
;;
;; 返回值
;; ------
;; integer?\n游戏区宽度（格子数）

(import (scheme base)
  (liii check)
  (liii tui snake logic)
) ;import

(check-set-mode! 'report-failed)

(check (snake-width (make-snake-game)) => 20)
(check (snake-width (make-snake-game :width 42)) => 42)
(check-catch 'type-error (snake-width "not-a-game"))

(check-report)
