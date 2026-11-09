;; snake-status
;; 读取游戏状态（running / paused / game-over）
;;
;; 语法
;; ----
;; (snake-status game)
;;
;; 参数
;; ----
;; game : snake-game?\n当前游戏状态
;;
;; 返回值
;; ------
;; symbol?\n游戏状态符号

(import (scheme base)
  (liii check)
  (liii tui snake logic)
) ;import

(check-set-mode! 'report-failed)

(check (snake-status (make-snake-game)) => 'running)
(check (snake-status (make-snake-game :status 'paused)) => 'paused)
(check (snake-status (make-snake-game :status 'game-over)) => 'game-over)
(check-catch 'type-error (snake-status 42))

(check-report)
