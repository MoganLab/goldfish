;; snake-height
;; 读取游戏区域的高度
;;
;; 语法
;; ----
;; (snake-height game)
;;
;; 参数
;; ----
;; game : snake-game?\n当前游戏状态
;;
;; 返回值
;; ------
;; integer?\n游戏区高度（格子数）

(import (scheme base)
  (liii check)
  (liii tui snake logic)
) ;import

(check-set-mode! 'report-failed)

(check (snake-height (make-snake-game)) => 12)
(check (snake-height (make-snake-game :height 25)) => 25)
(check-catch 'type-error (snake-height "not-a-game"))

(check-report)
