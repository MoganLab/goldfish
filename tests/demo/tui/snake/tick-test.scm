(import (liii check)
  (demo tui snake logic)
) ;import

(check-set-mode! 'report-failed)

;; 1. 正常步进移动测试
(let* ((g0 (make-snake-game :width 20 :height 12 :snake '((5 5) (4 5) (3 5)) :dir 'right))
       (g1 (snake-tick g0))
      ) ;
  (check (snake-head g1) => '(6 5))
  (check (snake-body g1) => '((6 5) (5 5) (4 5)))
  (check (snake-tick-count g1) => 1)
  (check (snake-status g1) => 'running)
) ;let*

;; 2. 边步进边转向测试
(let* ((g0 (make-snake-game :width 20 :height 12 :snake '((5 5) (4 5) (3 5)) :dir 'right))
       (g1 (snake-tick g0 'up))
      ) ;
  (check (snake-dir g1) => 'up)
  (check (snake-head g1) => '(5 4))
) ;let*

;; 3. 穿墙移动测试（右边缘穿越回左侧）
(let* ((g0 (make-snake-game :width 20 :height 12 :snake '((19 5) (18 5) (17 5)) :dir 'right))
       (g1 (snake-tick g0))
      ) ;
  (check (snake-head g1) => '(0 5))
  (check (snake-body g1) => '((0 5) (19 5) (18 5)))
  (check (snake-status g1) => 'running)
) ;let*

;; 4. 暂停与恢复测试
(let* ((g0 (make-snake-game :status 'running))
       (g1 (snake-tick g0 'space))
       (g2 (snake-tick g1))
       (g3 (snake-tick g2 'space))
      ) ;
  (check (snake-status g1) => 'paused)
  (check (snake-status g2) => 'paused)
  (check (snake-status g3) => 'running)
) ;let*

(check-report)
