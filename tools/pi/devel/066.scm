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
;; 任务 066: demo/tui/snake 示例配色 自动化验收脚本
;; ======================================================================

(display "=== 验收点 1: ANSI SGR 配色与宽度透明度 ===\n")
(with-color-mode #t
  (lambda ()
    (check (style-enabled?) => #t)
    ;; 验证蛇头、蛇身、食物、标题、数值高亮、暗调提示、转轮、GAME OVER 均含 SGR
    (let ((h (style-head "@"))
          (b (style-body "o"))
          (f (style-food "*"))
          (t (style-title " STATS "))
          (a (style-accent "99"))
          (d (style-dim "Help"))
          (hb (style-heartbeat "+"))
          (go (style-game-over "GAME OVER"))
         ) ;
      ;; 原始字符串含转义码
      (check (string-contains? h "\x1b;[") => #t)
      (check (string-contains? b "\x1b;[32m") => #t)
      (check (string-contains? f "\x1b;[1;31m") => #t)
      (check (string-contains? t "\x1b;[1;36m") => #t)
      (check (string-contains? a "\x1b;[1;33m") => #t)
      (check (string-contains? d "\x1b;[90m") => #t)
      (check (string-contains? hb "\x1b;[1;35m") => #t)
      (check (string-contains? go "\x1b;[1;31m") => #t)
      ;; 宽度透明度：显示宽度完全等于纯可见字符数
      (check (display-width h) => 1)
      (check (display-width b) => 1)
      (check (display-width f) => 1)
      (check (display-width t) => 7)
      (check (display-width a) => 2)
      (check (display-width d) => 4)
      (check (display-width hb) => 1)
      (check (display-width go) => 9)
      ;; 纯文本还原
      (check (strip-ansi h) => "@")
      (check (strip-ansi b) => "o")
      (check (strip-ansi f) => "*")
      (check (strip-ansi t) => " STATS ")
      (check (strip-ansi a) => "99")
      (check (strip-ansi d) => "Help")
      (check (strip-ansi hb) => "+")
      (check (strip-ansi go) => "GAME OVER")
    ) ;let
  ) ;lambda
) ;with-color-mode

(display "=== 验收点 2: NO_COLOR 与终端降级安全机制 ===\n")
(with-color-mode #f
  (lambda ()
    (check (style-enabled?) => #f)
    ;; 样式关闭时原样返回纯文本
    (check (style-head "@") => "@")
    (check (style-body "o") => "o")
    (check (style-food "*") => "*")
    (check (style-title " STATS ") => " STATS ")
    (check (style-accent "100") => "100")
    (check (style-dim "Text") => "Text")
    (check (style-heartbeat "x") => "x")
    (check (style-game-over "RECORD") => "RECORD")
    ;; 棋盘在无色模式下与历史无色基准严格一致
    (let* ((game (make-snake-game :width 10 :height 4 :snake '((2 1) (1 1)) :food '(5
                                                                                    1)))
           (grid (render-grid-lines game))
           (row1 (list-ref grid 1))
          ) ;
      (check (string-length row1) => 10)
      (check (string-ref row1 2) => #\@)
      (check (string-ref row1 1) => #\o)
      (check (string-ref row1 5) => #\*)
      (check (string-ref row1 0) => #\space)
    ) ;let*
  ) ;lambda
) ;with-color-mode

(display "=== 验收点 3: virtual-terminal 对 ANSI SGR 兼容性 ===\n")
(let ((vt (make-virtual-terminal :width 30 :height 5)))
  ;; 写入多段带色文本
  (terminal-write vt
    (string-append "\x1b;[1;32m@\x1b;[0m" "\x1b;[32moo\x1b;[0m" "  "
      "\x1b;[1;31m*\x1b;[0m"
    ) ;string-append
  ) ;terminal-write
  ;; 屏幕可见缓冲区绝无转义控制码，字符对齐在准确列位
  (let* ((scr (virtual-terminal-screen vt)) (row0 (car scr)))
    (check (string-ref row0 0) => #\@)
    (check (string-ref row0 1) => #\o)
    (check (string-ref row0 2) => #\o)
    (check (string-ref row0 3) => #\space)
    (check (string-ref row0 4) => #\space)
    (check (string-ref row0 5) => #\*)
    ;; 光标准确推进 6 列
    (check (terminal-cursor vt) => '(6 0))
  ) ;let*
  ;; 原始写入历史完整保留 ANSI SGR 转义序列
  (let ((raw (virtual-terminal-raw vt)))
    (check (pair? raw) => #t)
    (check (string-contains? (car raw) "\x1b;[1;32m") => #t)
    (check (string-contains? (car raw) "\x1b;[1;31m") => #t)
  ) ;let
) ;let

(display "=== 验收点 4: 差分渲染样式变更与零闪烁机制 ===\n")
(let* ((vt (make-virtual-terminal :width 40 :height 10))
       (screen (make-main-screen vt))
       ;; 帧 1：蛇头在 (3 1)，蛇身在 (2 1) (1 1)
       (frame1 '("Header" "\x1b;[32moo\x1b;[0m\x1b;[1;32m@\x1b;[0m" "Footer"))
       ;; 帧 2：蛇向前走一步，蛇头在 (4 1)，原蛇头 (3 1) 变为蛇身
       (frame2 '("Header" " \x1b;[32moo\x1b;[0m\x1b;[1;32m@\x1b;[0m" "Footer"))
      ) ;
  (screen-render! screen frame1)
  ;; 帧 1 提交后，清空 raw 日志以专门观察帧 2 的差分动作
  (let ((term-data (terminal-data vt)))
    (vt-raw-set! term-data '())
  ) ;let
  (screen-render! screen frame2)
  ;; 观察帧 2 的差分重绘动作：
  (let ((raw (virtual-terminal-raw vt)))
    ;; 1. 只有第 1 行发生变更，Header(0) 与 Footer(2) 应零写入 (keep)
    (check (string-contains? (apply string-append raw) "Header") => #f)
    (check (string-contains? (apply string-append raw) "Footer") => #f)
    ;; 2. 同步输出边界成对出现 (DECSET 2026)
    (check (not (not (member "\x1b;[?2026h" raw))) => #t)
    (check (not (not (member "\x1b;[?2026l" raw))) => #t)
    ;; 3. 原地覆写更新行，且无 clear-line 导致的空白窗
    (check (member "\x1b;[2K\r" raw) => #f)
  ) ;let
) ;let*

(display "=== 验收点 5: 完整游戏视图带颜色端到端装配 ===\n")
(with-color-mode #t
  (lambda ()
    (let* ((vt (make-virtual-terminal :width 60 :height 14))
           (screen (make-main-screen vt))
           (spinner (make-spinner))
           (game (make-snake-game
                   :width  20
                   :height 8
                   :snake  '((5 4) (4 4) (3 4))
                   :food   '(10 4)
                   :score  15
                 ) ;make-snake-game
           ) ;game
           (view (make-game-view game spinner))
           (lines (component-render view 60))
          ) ;
      (screen-render! screen lines)
      (let* ((scr (virtual-terminal-screen vt))
             (raw (apply string-append (virtual-terminal-raw vt)))
             (board-row4 (list-ref scr 5))
            ) ;
        ;; 1. 屏幕纯可见矩阵中蛇头 @ 与蛇身 o 在正确坐标（║ 占 3 字节）
        (check (substring board-row4 0 9) => "║   oo@")
        (check (substring board-row4 9 14) => "    *")
        ;; 2. 原始写入流中包含所有颜色的 SGR 转义码
        (check (string-contains? raw "\x1b;[1;32m") => #t)
        (check (string-contains? raw "\x1b;[32m") => #t)
        (check (string-contains? raw "\x1b;[1;31m") => #t)
        (check (string-contains? raw "\x1b;[1;36m") => #t)
        (check (string-contains? raw "\x1b;[1;33m") => #t)
        (check (string-contains? raw "\x1b;[1;35m") => #t)
      ) ;let*
    ) ;let*
  ) ;lambda
) ;with-color-mode

(check-report)
