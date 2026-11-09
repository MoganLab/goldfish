;; snake-high-score
;; 读取历史最高分
;;
;; 语法
;; ----
;; (snake-high-score game)
;;
;; 参数
;; ----
;; game : snake-game?\n当前游戏状态
;;
;; 返回值
;; ------
;; integer?\n历史最高分

(import (scheme base)
  (liii check)
  (liii tui snake logic)
) ;import

(check-set-mode! 'report-failed)

(check (snake-high-score (make-snake-game)) => 0)
(check (snake-high-score (make-snake-game :high-score 230)) => 230)
(check-catch 'type-error (snake-high-score #t))

(check-report)
