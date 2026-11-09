;; snake-spawn-food
;; 在避开蛇身的位置生成食物坐标
;;
;; 语法
;; ----
;; (snake-spawn-food width height body)
;;
;; 参数
;; ----
;; width, height : integer?\n游戏区尺寸；body : list?\n蛇身坐标列表
;;
;; 返回值
;; ------
;; list?\n食物坐标 (x y)，保证不在蛇身上

(import (scheme base)
  (liii check)
  (liii tui snake logic)
) ;import

(check-set-mode! 'report-failed)

;; 1. 默认策略：生成位置不在蛇身上
(let ((food (snake-spawn-food 20 12 '((5 5) (4 5) (3 5)))))
  (check (member food '((5 5) (4 5) (3 5))) => #f)
  (check (< (car food) 20) => #t)
  (check (< (cadr food) 12) => #t)
) ;let

;; 2. 满盘蛇身时回退到扫描第一个空格
(let ((food (snake-spawn-food 3 1 '((1 0) (2 0)))))
  (check food => '(0 0))
) ;let

;; 3. 全部占满时回退到 (0 0)
(check (snake-spawn-food 1 1 '((0 0))) => '(0 0))

(check-report)
