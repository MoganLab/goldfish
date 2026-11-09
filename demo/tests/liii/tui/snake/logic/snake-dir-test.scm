;; snake-dir
;; 读取蛇当前的前进方向
;;
;; 语法
;; ----
;; (snake-dir game)
;;
;; 参数
;; ----
;; game : snake-game?\n当前游戏状态
;;
;; 返回值
;; ------
;; symbol?\n方向：up / down / left / right

(import (scheme base)
  (liii check)
  (liii tui snake logic)
) ;import

(check-set-mode! 'report-failed)

(check (snake-dir (make-snake-game)) => 'right)
(check (snake-dir (make-snake-game :dir 'up)) => 'up)
(check-catch 'type-error (snake-dir '(1 2)))

(check-report)
