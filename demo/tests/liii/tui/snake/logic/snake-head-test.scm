;; snake-head
;; 读取蛇头坐标（蛇身列表首元素）
;;
;; 语法
;; ----
;; (snake-head game)
;;
;; 参数
;; ----
;; game : snake-game?\n当前游戏状态
;;
;; 返回值
;; ------
;; list?\n蛇头坐标 (x y)

(import (scheme base)
  (liii check)
  (liii tui snake logic)
) ;import

(check-set-mode! 'report-failed)

(check (snake-head (make-snake-game)) => '(5 5))
(check (snake-head (make-snake-game :snake '((9 8) (8 8)))) => '(9 8))
(check-catch 'type-error (snake-head #f))

(check-report)
