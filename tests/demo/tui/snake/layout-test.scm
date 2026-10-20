(import (liii check)
  (demo tui snake view)
) ;import

(check-set-mode! 'report-failed)

;; 1. 标准宽终端 (80x24)：水平并排布局，完整保留 STATS 与 HELP
(let ((layout (snake-compute-layout 80 24)))
  (check (cdr (assq 'board-width layout)) => 50)
  (check (cdr (assq 'board-height layout)) => 22)
  (check (cdr (assq 'layout layout)) => 'horizontal)
  (check (cdr (assq 'show-help? layout)) => #t)
) ;let

;; 2. 超大终端 (120x40)：命中最大尺寸上限
(let ((layout (snake-compute-layout 120 40)))
  (check (cdr (assq 'board-width layout)) => 50)
  (check (cdr (assq 'board-height layout)) => 25)
  (check (cdr (assq 'layout layout)) => 'horizontal)
  (check (cdr (assq 'show-help? layout)) => #t)
) ;let

;; 3. 中等宽度终端 (48x20)：水平布局，折叠 HELP 侧栏
(let ((layout (snake-compute-layout 48 20)))
  (check (cdr (assq 'board-width layout)) => 27)
  (check (cdr (assq 'board-height layout)) => 18)
  (check (cdr (assq 'layout layout)) => 'horizontal)
  (check (cdr (assq 'show-help? layout)) => #f)
) ;let

;; 4. 窄终端 (30x20)：纵向垂直布局
(let ((layout (snake-compute-layout 30 20)))
  (check (cdr (assq 'board-width layout)) => 28)
  (check (cdr (assq 'board-height layout)) => 11)
  (check (cdr (assq 'layout layout)) => 'vertical)
  (check (cdr (assq 'show-help? layout)) => #f)
) ;let

;; 5. 极小终端 (20x8)：紧凑纵向垂直布局
(let ((layout (snake-compute-layout 20 8)))
  (check (cdr (assq 'board-width layout)) => 18)
  (check (cdr (assq 'board-height layout)) => 4)
  (check (cdr (assq 'layout layout)) => 'vertical)
  (check (cdr (assq 'show-help? layout)) => #f)
) ;let

;; 6. snake-compute-dimensions 便捷函数
(check (snake-compute-dimensions 80 24) => '(50 22))
(check (snake-compute-dimensions 48 20) => '(27 18))
(check (snake-compute-dimensions 30 20) => '(28 11))

;; 7. 异常参数校验
(check-catch 'type-error (snake-compute-layout 0 24))
(check-catch 'type-error (snake-compute-layout 80 -1))
(check-catch 'type-error (snake-compute-layout "80" 24))
(check-catch 'type-error (snake-compute-dimensions -5 20))

(check-report)
