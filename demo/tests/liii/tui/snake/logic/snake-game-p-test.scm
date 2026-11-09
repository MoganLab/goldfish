;; snake-game?
;; 判断给定对象是否为贪吃蛇游戏状态记录
;;
;; 语法
;; ----
;; (snake-game? obj)
;;
;; 参数
;; ----
;; obj : any\n待检查的对象
;;
;; 返回值
;; ------
;; boolean?\n为 <snake-game> 记录时返回 #t，否则返回 #f

(import (scheme base)
  (liii check)
  (liii tui snake logic)
) ;import

(check-set-mode! 'report-failed)

(check-true (snake-game? (make-snake-game)))
(check-false (snake-game? "not-a-game"))
(check-false (snake-game? 123))
(check-false (snake-game? '()))
(check-false (snake-game? #f))

(check-report)
