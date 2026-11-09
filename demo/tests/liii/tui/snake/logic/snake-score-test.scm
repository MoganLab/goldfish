;; snake-score
;; 读取当前得分
;;
;; 语法
;; ----
;; (snake-score game)
;;
;; 参数
;; ----
;; game : snake-game?\n当前游戏状态
;;
;; 返回值
;; ------
;; integer?\n当前得分

(import (scheme base)
  (liii check)
  (liii tui snake logic)
) ;import

(check-set-mode! 'report-failed)

(check (snake-score (make-snake-game)) => 0)
(check (snake-score (make-snake-game :score 80)) => 80)
(check-catch 'type-error (snake-score '()))

(check-report)
