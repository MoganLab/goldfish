;; snake-body
;; 读取蛇身坐标列表（首元素为蛇头）
;;
;; 语法
;; ----
;; (snake-body game)
;;
;; 参数
;; ----
;; game : snake-game?\n当前游戏状态
;;
;; 返回值
;; ------
;; list?\n蛇身坐标列表，每项为 (x y)

(import (scheme base)
  (liii check)
  (liii tui snake logic)
) ;import

(check-set-mode! 'report-failed)

(check (snake-body (make-snake-game)) => '((5 5) (4 5) (3 5)))
(check (snake-body (make-snake-game :snake '((0 0)))) => '((0 0)))
(check-catch 'type-error (snake-body 'not-a-game))

(check-report)
