(import (liii check)
  (demo tui snake logic)
) ;import

(check-set-mode! 'report-failed)

;; 1. 相同尺寸 resize 返回原游戏对象
(let* ((g0 (make-snake-game :width 20 :height 10))
       (g1 (snake-resize g0 20 10))
      ) ;
  (check (snake-width g1) => 20)
  (check (snake-height g1) => 10)
  (check (snake-body g1) => (snake-body g0))
  (check (snake-food g1) => (snake-food g0))
) ;let*

;; 2. 放大棋盘：蛇身与食物坐标完整保留，状态与分数保留
(let* ((body '((5 5) (4 5) (3 5)))
       (g0 (make-snake-game
             :width      20
             :height     10
             :snake      body
             :dir        'up
             :food       '(12 8)
             :score      50
             :high-score 150
             :status     'running
             :tick-count 25
           ) ;make-snake-game
       ) ;g0
       (g1 (snake-resize g0 40 20))
      ) ;
  (check (snake-width g1) => 40)
  (check (snake-height g1) => 20)
  (check (snake-body g1) => '((5 5) (4 5) (3 5)))
  (check (snake-food g1) => '(12 8))
  (check (snake-dir g1) => 'up)
  (check (snake-score g1) => 50)
  (check (snake-high-score g1) => 150)
  (check (snake-status g1) => 'running)
  (check (snake-tick-count g1) => 25)
) ;let*

;; 3. 缩小棋盘：超出边界的身体节安全取模
(let* ((body '((15 8) (14 8) (13 8)))
       (g0 (make-snake-game :width 20 :height 10 :snake body :food '(2 2)))
       (g1 (snake-resize g0 10 5))
      ) ;
  (check (snake-width g1) => 10)
  (check (snake-height g1) => 5)
  ;; (15 8) mod (10 5) = (5 3), (14 8) = (4 3), (13 8) = (3 3)
  (check (snake-body g1) => '((5 3) (4 3) (3 3)))
  (check (snake-food g1) => '(2 2))
) ;let*

;; 4. 缩小棋盘：取模后重复身体节去重，避免折叠自碰
(let* ((body '((5 2) (15 2) (5 12)))
       (g0 (make-snake-game :width 20 :height 20 :snake body :food '(0 0)))
       (g1 (snake-resize g0 10 10))
       (new-body (snake-body g1))
      ) ;
  ;; 取模后原本为 ((5 2) (5 2) (5 2))，去重后只保留第一节 (5 2)
  (check new-body => '((5 2)))
  (check (snake-head g1) => '(5 2))
) ;let*

;; 5. 食物重叠防冲突：食物原本在 (15 5)，缩到 10x10 取模为 (5 5)，恰好与蛇头 (5 5) 重叠
;; snake-resize 应自动重新为食物寻找非身体合法空格
(let* ((body '((5 5) (4 5)))
       (g0 (make-snake-game :width 20 :height 10 :snake body :food '(15 5)))
       (g1 (snake-resize g0 10 10))
       (new-food (snake-food g1))
       (new-body (snake-body g1))
      ) ;
  (check (not (equal? new-food '(5 5))) => #t)
  (check (not (member new-food new-body)) => #t)
  (check (< (car new-food) 10) => #t)
  (check (< (cadr new-food) 10) => #t)
) ;let*

;; 6. 异常参数校验抛出 type-error
(let ((g0 (make-snake-game)))
  (check-catch 'type-error (snake-resize "not-a-game" 10 10))
  (check-catch 'type-error (snake-resize g0 0 10))
  (check-catch 'type-error (snake-resize g0 10 -5))
  (check-catch 'type-error (snake-resize g0 "10" 10))
) ;let

(check-report)
