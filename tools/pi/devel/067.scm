(import (scheme base)
  (scheme process-context)
  (liii check)
  (liii string)
  (liii tui component)
  (liii tui box)
  (liii tui loader)
  (liii tui editor)
  (liii tui text-width)
  (liii tui terminal)
  (liii tui terminal protocol)
  (liii tui terminal virtual state)
  (liii tui main-screen)
  (demo tui snake logic)
  (demo tui snake view)
  (demo tui snake style)
) ;import

(check-set-mode! 'report-failed)

;; ======================================================================
;; 任务 067: 贪吃蛇棋盘随终端尺寸自适应 自动化验收脚本
;; ======================================================================

(display "=== 验收点 1: 自适应布局纯函数计算与多尺寸覆盖 ===\n"
) ;display
;; 1. 宽终端 (80x24)：水平并排布局，完整保留 STATS 与 HELP
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

(display "=== 验收点 2: 逻辑层状态缩放算法 (snake-resize) ===\n")
;; 相同尺寸原样返回
(let* ((g0 (make-snake-game :width 20 :height 10)) (g1 (snake-resize g0 20 10)))
  (check (snake-width g1) => 20)
  (check (snake-height g1) => 10)
  (check (snake-body g1) => (snake-body g0))
) ;let*

;; 放大棋盘：蛇身与食物保留，属性完整继承
(let* ((body '((5 5) (4 5) (3 5)))
       (g0 (make-snake-game
             :width      20
             :height     10
             :snake      body
             :dir        'left
             :food       '(12 8)
             :score      80
             :high-score 160
             :status     'running
             :tick-count 40
           ) ;make-snake-game
       ) ;g0
       (g1 (snake-resize g0 40 20))
      ) ;
  (check (snake-width g1) => 40)
  (check (snake-height g1) => 20)
  (check (snake-body g1) => body)
  (check (snake-food g1) => '(12 8))
  (check (snake-dir g1) => 'left)
  (check (snake-score g1) => 80)
  (check (snake-high-score g1) => 160)
  (check (snake-status g1) => 'running)
  (check (snake-tick-count g1) => 40)
) ;let*

;; 缩小棋盘：超出边界的身体节安全取模与去重
(let* ((body '((15 8) (5 8) (15 18)))
       (g0 (make-snake-game :width 20 :height 20 :snake body :food '(5 8)))
       (g1 (snake-resize g0 10 10))
       (new-body (snake-body g1))
       (new-food (snake-food g1))
      ) ;
  ;; (15 8) mod 10 = (5 8); (5 8) mod 10 = (5 8); (15 18) mod 10 = (5 8)
  ;; 去重后仅保留第一节
  (check new-body => '((5 8)))
  ;; 原食物在 (5 8) 与新蛇身重叠，已被安全重放置
  (check (not (equal? new-food '(5 8))) => #t)
  (check (not (member new-food new-body)) => #t)
) ;let*

;; 异常参数校验
(let ((g (make-snake-game)))
  (check-catch 'type-error (snake-resize "bad" 10 10))
  (check-catch 'type-error (snake-resize g 0 10))
  (check-catch 'type-error (snake-resize g 10 -2))
) ;let

(display "=== 验收点 3: virtual-terminal 放大自适应与全量重绘 ===\n"
) ;display
(let* ((vt (make-virtual-terminal :width 48 :height 16))
       (screen (make-main-screen vt))
       (spinner (make-spinner))
       ;; 初始中等宽度
       (init-dims (snake-compute-dimensions 48 16))
       (game (make-snake-game :width (car init-dims) :height (cadr init-dims)))
       (view1 (make-game-view game spinner 48 16))
      ) ;
  (screen-render! screen (component-render view1 48))
  (check (terminal-size vt) => '(48 16))
  ;; 放大到 80x24
  (let* ((new-cols 80)
         (new-rows 24)
         (new-dims (snake-compute-dimensions new-cols new-rows))
         (resized-game (snake-resize game (car new-dims) (cadr new-dims)))
        ) ;
    (screen-resize! screen new-cols new-rows)
    (check (terminal-size vt) => '(80 24))
    ;; 用新尺寸渲染
    (let* ((view2 (make-game-view resized-game spinner new-cols new-rows))
           (lines2 (component-render view2 new-cols))
          ) ;
      (screen-render! screen lines2)
      (let* ((scr (virtual-terminal-screen vt))
             (first-row (car scr))
             (raw (apply string-append (virtual-terminal-raw vt)))
            ) ;
        ;; 屏幕宽度与行数完全更新
        (check (length scr) => 24)
        (check (string-length first-row) => 80)
        ;; 放大后右侧包含 HELP 标题
        (check (string-contains? raw "HELP") => #t)
      ) ;let*
    ) ;let*
  ) ;let*
) ;let*

(display "=== 验收点 4: virtual-terminal 缩小自适应与布局切换 ===\n"
) ;display
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (screen (make-main-screen vt))
       (spinner (make-spinner))
       (game (make-snake-game :width 50 :height 22))
       (view1 (make-game-view game spinner 80 24))
      ) ;
  (screen-render! screen (component-render view1 80))
  ;; 缩小到窄终端 30x20
  (let* ((new-cols 30)
         (new-rows 20)
         (new-dims (snake-compute-dimensions new-cols new-rows))
         (resized-game (snake-resize game (car new-dims) (cadr new-dims)))
        ) ;
    (screen-resize! screen new-cols new-rows)
    (check (terminal-size vt) => '(30 20))
    (let* ((view2 (make-game-view resized-game spinner new-cols new-rows))
           (lines2 (component-render view2 new-cols))
          ) ;
      ;; 清空 resize 重绘过渡产生的 raw，专注于断言新帧写入
      (let ((term-data (terminal-data vt)))
        (vt-raw-set! term-data '())
      ) ;let
      (screen-render! screen lines2)
      (let* ((scr (virtual-terminal-screen vt))
             (first-row (car scr))
             (raw (apply string-append (virtual-terminal-raw vt)))
            ) ;
        (check (length scr) => 20)
        (check (string-length first-row) => 30)
        ;; 窄终端垂直排列，当前屏幕与新帧写入中 STATS 存在，HELP 被折叠
        (check (string-contains? raw "STATS") => #t)
        (check (string-contains? raw "Arrows / WASD") => #f)
        (check (any (lambda (row) (string-contains? row "Arrows / WASD")) scr) => #f)
      ) ;let*
    ) ;let*
  ) ;let*
) ;let*

(display "=== 验收点 5: resize 差分渲染与零闪烁机制保持 ===\n")
(let* ((vt (make-virtual-terminal :width 50 :height 15))
       (screen (make-main-screen vt))
       (spinner (make-spinner))
       (game (make-snake-game :width 20 :height 8))
       (view1 (make-game-view game spinner 50 15))
      ) ;
  (screen-render! screen (component-render view1 50))
  ;; 清空 raw 记录
  (let ((term-data (terminal-data vt)))
    (vt-raw-set! term-data '())
  ) ;let
  ;; 尺寸调整
  (screen-resize! screen 60 18)
  (let* ((view2 (make-game-view game spinner 60 18))
         (lines2 (component-render view2 60))
        ) ;
    (screen-render! screen lines2)
    (let ((raw (virtual-terminal-raw vt)))
      ;; 同步输出标记成对出现 (DECSET 2026)
      (check (not (not (member "\x1b;[?2026h" raw))) => #t)
      (check (not (not (member "\x1b;[?2026l" raw))) => #t)
      ;; 杜绝在更新行前清空整行的空白闪烁操作
      (check (member "\x1b;[2K\r" raw) => #f)
    ) ;let
  ) ;let*
) ;let*

(check-report)
