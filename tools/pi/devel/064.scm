(import (liii check)
  (liii string)
  (liii tui text-width)
  (liii tui terminal)
  (liii tui component)
  (liii tui box)
  (liii tui main-screen)
) ;import

(check-set-mode! 'report-failed)

;; 任务 064: (liii tui) 差分渲染闪烁修复 自动化验收脚本
;; 验证原地覆写、行尾清除、按需光标控制、DECSET 2026 同步包裹与逐写入零空白中间态

(display "=== 验收点 1: 终端协议扩展（行尾清除与 DECSET 2026 同步输出） ===\n"
) ;display
(let ((vt (make-virtual-terminal :width 15 :height 3)))
  ;; 1.1 terminal-begin-sync! 与 terminal-end-sync!
  (terminal-begin-sync! vt)
  (check (car (virtual-terminal-raw vt)) => "\x1b;[?2026h")
  (terminal-end-sync! vt)
  (check (cadr (virtual-terminal-raw vt)) => "\x1b;[?2026l")

  ;; 1.2 terminal-clear-line-to-end
  (terminal-write vt "0123456789")
  (terminal-set-cursor! vt 4 0)
  (terminal-clear-line-to-end vt)
  (check (car (virtual-terminal-screen vt)) => "0123           ")
  (check (car (reverse (virtual-terminal-raw vt))) => "\x1b;[K")

  ;; 1.3 类型错误检查
  (check-catch 'type-error (terminal-clear-line-to-end 'invalid))
  (check-catch 'type-error (terminal-begin-sync! 'invalid))
  (check-catch 'type-error (terminal-end-sync! 'invalid))
) ;let

(display "=== 验收点 2: 标准终端 ANSI 序列与智能按需光标可见性 ===\n"
) ;display
(let* ((p (open-output-string))
       (st (make-standard-terminal :output-port p))
       (screen (make-main-screen st :auto-hide-cursor #t))
      ) ;
  ;; 2.1 第 1 帧：光标由显示变为隐藏，发送一次 \x1b;[?25l，成对 DECSET 2026
  (screen-render! screen '("Frame One"))
  (let ((out1 (get-output-string p)))
    (check (string-contains? out1 "\x1b;[?25l") => #t)
    (check (string-contains? out1 "\x1b;[?2026h") => #t)
    (check (string-contains? out1 "\x1b;[?2026l") => #t)
  ) ;let

  ;; 2.2 第 2 帧：光标已隐藏，绝不再重复发送 \x1b;[?25l
  (let* ((p2 (open-output-string))
         (st2 (make-standard-terminal :output-port p2))
         (screen2 (make-main-screen st2 :auto-hide-cursor #t))
        ) ;
    (screen-render! screen2 '("Initial Frame"))
    (let ((len-before (string-length (get-output-string p2))))
      (screen-render! screen2 '("Second Frame"))
      (let* ((all-out (get-output-string p2))
             (frame2-out (substring all-out len-before (string-length all-out)))
            ) ;
        ;; 断言第 2 帧零重复 hide-cursor
        (check (string-contains? frame2-out "\x1b;[?25l") => #f)
        ;; 断言包含同步输出标记
        (check (string-contains? frame2-out "\x1b;[?2026h") => #t)
        (check (string-contains? frame2-out "\x1b;[?2026l") => #t)
        ;; 断言更新行不包含 \x1b;[2K\r 清行
        (check (string-contains? frame2-out "\x1b;[2K\r") => #f)
      ) ;let*
    ) ;let
  ) ;let*
) ;let*

(display "=== 验收点 3: 原地覆写与清除行尾（消除先清后写空白窗） ===\n"
) ;display
(let* ((vt (make-virtual-terminal :width 20 :height 3))
       (screen (make-main-screen vt))
      ) ;
  ;; 3.1 初始帧
  (screen-render! screen '("Alpha Beta Gamma" "Row 2: steady"))
  (check (virtual-terminal-screen vt)
    =>
    '("Alpha Beta Gamma    " "Row 2: steady       " "                    ")
  ) ;check

  ;; 3.2 等宽更新：完全原地覆写，无任何清空指令
  (let ((raw-count-1 (length (virtual-terminal-raw vt))))
    (screen-render! screen '("Alpha Beta Delta" "Row 2: steady"))
    (let ((diff-raw (list-tail (virtual-terminal-raw vt) raw-count-1)))
      (check diff-raw => '("\x1b;[?2026h" "Alpha Beta Delta" "\x1b;[?2026l"))
      (check (member "\x1b;[2K\r" diff-raw) => #f)
      (check (member "\x1b;[K" diff-raw) => #f)
    ) ;let
    (check (virtual-terminal-screen vt)
      =>
      '("Alpha Beta Delta    " "Row 2: steady       " "                    ")
    ) ;check
  ) ;let

  ;; 3.3 缩短更新：原地覆写新内容，在新内容末尾调用 clear-line-to-end 擦除残余
  (let ((raw-count-2 (length (virtual-terminal-raw vt))))
    (screen-render! screen '("Short" "Row 2: steady"))
    (let ((diff-raw (list-tail (virtual-terminal-raw vt) raw-count-2)))
      (check diff-raw => '("\x1b;[?2026h" "Short" "\x1b;[K" "\x1b;[?2026l"))
      ;; 严禁出现整行全清序列
      (check (member "\x1b;[2K\r" diff-raw) => #f)
    ) ;let
    ;; 验证尾部旧字符残留被正确清除为空格
    (check (car (virtual-terminal-screen vt)) => "Short               ")
  ) ;let

  ;; 3.4 内容无变动：零写入零 I/O
  (let ((raw-count-3 (length (virtual-terminal-raw vt))))
    (screen-render! screen '("Short" "Row 2: steady"))
    (check (length (virtual-terminal-raw vt)) => raw-count-3)
  ) ;let
) ;let*

(display "=== 验收点 4: 逐写入重绘模拟器（中间态零全空行断言） ===\n"
) ;display
(let* ((vt (make-virtual-terminal :width 24 :height 4))
       (screen (make-main-screen vt))
       (row1-snapshots '())
      ) ;
  (screen-render! screen
    '("┌──────────────────────┐"
      "│ Heartbeat:   123     │" "│ Score:       000     │"
      "└──────────────────────┘")
  ) ;screen-render!
  ;; 构建拦截所有底层原子写入动作的包装终端
  (let ((wrapped-term
          (make-terminal-record 'virtual
            (lambda (t) (terminal-size vt))
            (lambda (t text)
              (terminal-write vt text)
              (set! row1-snapshots
                (cons (list-ref (virtual-terminal-screen vt) 1) row1-snapshots)
              ) ;set!
            ) ;lambda
            (lambda (t) (terminal-cursor vt))
            (lambda (t x y) (terminal-set-cursor! vt x y))
            (lambda (t) (terminal-clear vt))
            (lambda (t)
              (terminal-clear-line vt)
              (set! row1-snapshots
                (cons (list-ref (virtual-terminal-screen vt) 1) row1-snapshots)
              ) ;set!
            ) ;lambda
            (lambda (t)
              (terminal-clear-line-to-end vt)
              (set! row1-snapshots
                (cons (list-ref (virtual-terminal-screen vt) 1) row1-snapshots)
              ) ;set!
            ) ;lambda
            (lambda (t) (terminal-begin-sync! vt))
            (lambda (t) (terminal-end-sync! vt))
            (lambda (t) (terminal-cursor-visible? vt))
            (lambda (t v) (if v (terminal-show-cursor! vt) (terminal-hide-cursor! vt)))
            (lambda (t) (terminal-flush vt))
            #f
          ) ;make-terminal-record
        ) ;wrapped-term
       ) ;
    (let ((wrapped-screen (make-main-screen wrapped-term)))
      (screen-buffer-commit! (screen-buffer wrapped-screen)
        '("┌──────────────────────┐"
          "│ Heartbeat:   123     │" "│ Score:       000     │"
          "└──────────────────────┘")
      ) ;screen-buffer-commit!
      ;; 模拟下一帧 Heartbeat 动画跳变
      (screen-render! wrapped-screen
        '("┌──────────────────────┐"
          "│ Heartbeat:   456     │" "│ Score:       000     │"
          "└──────────────────────┘")
      ) ;screen-render!
      ;; 断言：捕获到的所有中间快照中，Heartbeat 所在行绝对不为空白行！
      (let ((blank-row (make-string 24 #\space)))
        (check (pair? row1-snapshots) => #t)
        (for-each (lambda (snap) (check (string=? snap blank-row) => #f))
          row1-snapshots
        ) ;for-each
      ) ;let
    ) ;let
  ) ;let
) ;let*

(display "=== 验收点 5: 真实 TUI 复杂组件渲染与零闪烁结合 ===\n"
) ;display
(let* ((vt (make-virtual-terminal :width 30 :height 5))
       (screen (make-main-screen vt))
       (box-v1 (make-box "Score: 10" :title "Game" :style 'single))
       (box-v2 (make-box "Score: 20" :title "Game" :style 'single))
      ) ;
  (screen-render! screen box-v1)
  (check (string-contains? (cadr (virtual-terminal-screen vt)) "Score: 10") => #t)
  (let ((raw-before (length (virtual-terminal-raw vt))))
    (screen-render! screen box-v2)
    (let ((diff-raw (list-tail (virtual-terminal-raw vt) raw-before)))
      ;; 只有更新行，首尾被同步模式包裹
      (check (car diff-raw) => "\x1b;[?2026h")
      (check (car (reverse diff-raw)) => "\x1b;[?2026l")
      ;; 无全行清空
      (check (member "\x1b;[2K\r" diff-raw) => #f)
    ) ;let
    (check (string-contains? (cadr (virtual-terminal-screen vt)) "Score: 20") => #t)
  ) ;let
) ;let*

(check-report)
