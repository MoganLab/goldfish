(import (liii check)
  (demo tui snake logic)
) ;import

(check-set-mode! 'report-failed)

;; 1. 吃到食物测试：得分增加、长度增加 1、旧尾部不丢弃
(let* ((g0 (make-snake-game :width 20 :height 12 :snake '((5 5) (4 5) (3 5))
             :dir 'right :food '(6 5) :score 0 :high-score 0
           ) ;make-snake-game
       ) ;g0
       (g1 (snake-tick g0))
      ) ;
  (check (snake-head g1) => '(6 5))
  ;; 身体长度由 3 增至 4
  (check (length (snake-body g1)) => 4)
  (check (car (snake-body g1)) => '(6 5))
  (check (snake-score g1) => 10)
  (check (snake-high-score g1) => 10)
  ;; 新食物已重新生成在非蛇身位置
  (check (member (snake-food g1) (snake-body g1)) => #f)
) ;let*

;; 2. 连续吃两个食物测试
(let* ((g0 (make-snake-game :width 20 :height 12 :snake '((5 5) (4 5) (3 5))
             :dir 'right :food '(6 5) :score 20 :high-score 50
           ) ;make-snake-game
       ) ;g0
       (g1 (snake-tick g0))
      ) ;
  (check (snake-score g1) => 30)
  (check (snake-high-score g1) => 50)
) ;let*

(check-report)
