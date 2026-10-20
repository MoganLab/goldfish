(import (liii check)
  (liii string)
  (liii tui component)
  (liii tui box)
  (liii tui terminal)
  (liii tui main-screen)
) ;import

(check-set-mode! 'report-failed)

;; 任务 062: (liii tui main-screen) 自动化验收脚本
;; 基于 virtual-terminal 与 standard-terminal 全流程差分渲染验证

;; 1. 纯函数行差分算法验证
(let ((d (screen-diff '("a" "b" "c") '("a" "x" "c" "d"))))
  (check (diff-op-type (car d)) => 'keep)
  (check (diff-op-type (cadr d)) => 'update)
  (check (diff-op-text (cadr d)) => "x")
  (check (diff-op-type (caddr d)) => 'keep)
  (check (diff-op-type (cadddr d)) => 'append)
  (check (diff-op-text (cadddr d)) => "d")
) ;let

;; 2. 帧缓冲区状态转换验证
(let ((buf (make-screen-buffer)))
  (check (screen-buffer-dirty? buf) => #t)
  (screen-buffer-commit! buf '("frame 1"))
  (check (screen-buffer-dirty? buf) => #f)
  (check (screen-buffer-lines buf) => '("frame 1"))
  (screen-buffer-commit! buf '("frame 2"))
  (check (screen-buffer-prev-lines buf) => '("frame 1"))
  (screen-buffer-invalidate! buf)
  (check (screen-buffer-dirty? buf) => #t)
) ;let

;; 3. 基于 Virtual Terminal 的差分渲染与零闪烁验证
(let* ((vt (make-virtual-terminal :width 20 :height 5))
       (screen (make-main-screen vt))
      ) ;
  ;; 3.1 初始帧渲染
  (screen-render! screen
    '("row 0: ready" "row 1: status ok" "row 2: idle" "row 3: count 0"
      "row 4: end")
  ) ;screen-render!
  ;; 首帧包含 begin-sync, 5 行文本写入, end-sync 共 7 项
  (check (length (virtual-terminal-raw vt)) => 7)
  (check (virtual-terminal-screen vt)
    =>
    '("row 0: ready        " "row 1: status ok    " "row 2: idle         "
      "row 3: count 0      " "row 4: end          ")
  ) ;check

  ;; 3.2 零写入测试（相同帧内容重绘，完全零 IO 零写入）
  (let ((raw-count-before (length (virtual-terminal-raw vt))))
    (screen-render! screen
      '("row 0: ready" "row 1: status ok" "row 2: idle" "row 3: count 0"
        "row 4: end")
    ) ;screen-render!
    (check (length (virtual-terminal-raw vt)) => raw-count-before)
    (check (virtual-terminal-screen vt)
      =>
      '("row 0: ready        " "row 1: status ok    " "row 2: idle         "
        "row 3: count 0      " "row 4: end          ")
    ) ;check
  ) ;let

  ;; 3.3 最小单行更新测试（仅第 3 行数字变更，底层仅发生 1 次写入）
  (let ((raw-count-before (length (virtual-terminal-raw vt))))
    (screen-render! screen
      '("row 0: ready" "row 1: status ok" "row 2: idle" "row 3: count 1"
        "row 4: end")
    ) ;screen-render!
    ;; 帧包裹 begin-sync, 1 次更新写入, end-sync 共新增 3 项
    (check (length (virtual-terminal-raw vt)) => (+ raw-count-before 3))
    (check (list-ref (virtual-terminal-raw vt) (+ raw-count-before 1))
      =>
      "row 3: count 1"
    ) ;check
    (check (virtual-terminal-screen vt)
      =>
      '("row 0: ready        " "row 1: status ok    " "row 2: idle         "
        "row 3: count 1      " "row 4: end          ")
    ) ;check
  ) ;let

  ;; 3.4 行数缩减与屏幕残余行清除测试
  (screen-render! screen '("row 0: only" "row 1: two rows"))
  (check (virtual-terminal-screen vt)
    =>
    '("row 0: only         " "row 1: two rows     " "                    "
      "                    " "                    ")
  ) ;check

  ;; 3.5 光标定位与光标显隐控制
  (screen-render! screen '("line with cursor") '(5 0))
  (check (terminal-cursor vt) => '(5 0))
  (check (terminal-cursor-visible? vt) => #t)

  ;; 3.6 screen-clear! 彻底清屏
  (screen-clear! screen)
  (check (screen-lines screen) => '())
  (check (virtual-terminal-screen vt)
    =>
    '("                    " "                    " "                    "
      "                    " "                    ")
  ) ;check
) ;let*

;; 4. 终端几何尺寸变更 (Resize) 与自适应全量重绘
(let* ((vt (make-virtual-terminal :width 12 :height 3))
       (screen (make-main-screen vt))
      ) ;
  (screen-render! screen '("Alpha" "Beta"))
  (check (virtual-terminal-screen vt)
    =>
    '("Alpha       " "Beta        " "            ")
  ) ;check

  ;; 触发 resize
  (screen-resize! screen 16 4)
  (check (terminal-size vt) => '(16 4))
  ;; 验证按新尺寸 16x4 重新绘制
  (check (virtual-terminal-screen vt)
    =>
    '("Alpha           " "Beta            " "                "
      "                ")
  ) ;check

  ;; screen-refresh! 强制全量重绘
  (screen-refresh! screen)
  (check (virtual-terminal-screen vt)
    =>
    '("Alpha           " "Beta            " "                "
      "                ")
  ) ;check
) ;let*

;; 5. 真实 ANSI 序列输出验证（通过 Standard Terminal）
(let* ((out-port (open-output-string))
       (st (make-standard-terminal :output-port out-port))
       (screen (make-main-screen st))
      ) ;
  (screen-render! screen '("Hello ANSI" "World ANSI"))
  (let ((ansi (get-output-string out-port)))
    (check (> (string-length ansi) 0) => #t)
    ;; 验证光标定位转义码 \x1b[y;xH
    (check (string-contains? ansi "\x1b;[") => #t)
    ;; 验证 DECSET 2026 同步转义码
    (check (string-contains? ansi "\x1b;[?2026h") => #t)
    (check (string-contains? ansi "\x1b;[?2026l") => #t)
    ;; 验证渲染文本包含其中
    (check (string-contains? ansi "Hello ANSI") => #t)
    (check (string-contains? ansi "World ANSI") => #t)
  ) ;let
) ;let*

;; 6. TUI 容器组件直接驱动屏幕渲染
(let* ((vt (make-virtual-terminal :width 18 :height 4))
       (screen (make-main-screen vt))
       (box-comp (make-box "TUI Box" :style 'single :title "Win"))
      ) ;
  (screen-render! screen box-comp)
  (let ((scr (virtual-terminal-screen vt)))
    (check (string-contains? (car scr) "Win") => #t)
    (check (string-contains? (cadr scr) "TUI Box") => #t)
  ) ;let
) ;let*

(check-report)
