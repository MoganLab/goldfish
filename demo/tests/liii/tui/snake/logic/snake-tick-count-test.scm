;; snake-tick-count
;; 读取已推进的帧数
;;
;; 语法
;; ----
;; (snake-tick-count game)
;;
;; 参数
;; ----
;; game : snake-game?\n当前游戏状态
;;
;; 返回值
;; ------
;; integer?\n已推进帧数

(import (scheme base)
  (liii check)
  (liii tui snake logic)
) ;import

(check-set-mode! 'report-failed)

(check (snake-tick-count (make-snake-game)) => 0)
(check (snake-tick-count (make-snake-game :tick-count 17)) => 17)
(check-catch 'type-error (snake-tick-count "g"))

(check-report)
