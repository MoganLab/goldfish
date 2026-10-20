(import (liii check)
  (demo tui snake logic)
) ;import

(check-set-mode! 'report-failed)

;; 1. 正常转向测试
(let ((g (make-snake-game :dir 'right)))
  (check (snake-dir (snake-turn g 'up)) => 'up)
  (check (snake-dir (snake-turn g 'down)) => 'down)
) ;let

;; 2. WASD 字符映射转向测试
(let ((g (make-snake-game :dir 'right)))
  (check (snake-dir (snake-turn g "w")) => 'up)
  (check (snake-dir (snake-turn g "W")) => 'up)
  (check (snake-dir (snake-turn g "s")) => 'down)
  (check (snake-dir (snake-turn g "S")) => 'down)
) ;let

;; 3. 反向保护测试（不能直接掉头）
(let ((g (make-snake-game :dir 'right)))
  ;; 向右运动时向左转向无效，保持向右
  (check (snake-dir (snake-turn g 'left)) => 'right)
  (check (snake-dir (snake-turn g "a")) => 'right)
) ;let

(let ((g (make-snake-game :dir 'up)))
  ;; 向上运动时向下转向无效，保持向上
  (check (snake-dir (snake-turn g 'down)) => 'up)
  (check (snake-dir (snake-turn g "s")) => 'up)
) ;let

(check-report)
