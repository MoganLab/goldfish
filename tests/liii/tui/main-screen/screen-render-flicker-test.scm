(import (liii check)
  (liii string)
  (liii tui text-width)
  (liii tui terminal)
  (liii tui main-screen)
) ;import

(check-set-mode! 'report-failed)

;; 任务 064 防闪烁与逐写入重绘模拟器测试

;; 1. 同步输出边界成对出现断言（virtual-terminal）
(let* ((vt (make-virtual-terminal :width 20 :height 3))
       (screen (make-main-screen vt))
      ) ;
  (screen-render! screen '("line 1" "line 2"))
  (let ((raw (virtual-terminal-raw vt)))
    ;; 首项为 begin-sync 2026h，尾项为 end-sync 2026l
    (check (car raw) => "\x1b;[?2026h")
    (check (car (reverse raw)) => "\x1b;[?2026l")
  ) ;let
) ;let*

;; 2. 等宽行更新：原地覆写，不产生任何 clear 序列（既无 \x1b[2K 也无 \x1b[K）
(let* ((vt (make-virtual-terminal :width 20 :height 3))
       (screen (make-main-screen vt))
      ) ;
  (screen-render! screen '("old-line-1" "old-line-2"))
  (let ((raw-before (length (virtual-terminal-raw vt))))
    ;; 更新第 2 行，等长文本
    (screen-render! screen '("old-line-1" "new-line-2"))
    (let* ((all-raw (virtual-terminal-raw vt)) (frame-raw (list-tail all-raw raw-before)))
      ;; 帧内包含 begin-sync, new text, end-sync
      (check frame-raw => '("\x1b;[?2026h" "new-line-2" "\x1b;[?2026l"))
      ;; 断言不包含任何 clear 序列
      (check (member "\x1b;[2K\r" frame-raw) => #f)
      (check (member "\x1b;[K" frame-raw) => #f)
    ) ;let*
  ) ;let
) ;let*

;; 3. 缩短行更新：原地覆写后仅在尾部调用 clear-line-to-end (\x1b[K)
(let* ((vt (make-virtual-terminal :width 20 :height 3))
       (screen (make-main-screen vt))
      ) ;
  (screen-render! screen '("long-content"))
  (let ((raw-before (length (virtual-terminal-raw vt))))
    ;; 更新为短文本
    (screen-render! screen '("short"))
    (let* ((all-raw (virtual-terminal-raw vt)) (frame-raw (list-tail all-raw raw-before)))
      ;; 包含 begin-sync, short, clear-to-end, end-sync
      (check frame-raw => '("\x1b;[?2026h" "short" "\x1b;[K" "\x1b;[?2026l"))
      ;; 验证 virtual-terminal-screen 尾部被正确填充为空格
      (check (car (virtual-terminal-screen vt)) => "short               ")
      ;; 断言绝对不含整行清空序列 \x1b[2K\r
      (check (member "\x1b;[2K\r" frame-raw) => #f)
    ) ;let*
  ) ;let
) ;let*

;; 4. 标准终端 ANSI 流断言：按需光标控制与无重复 hide-cursor
(let* ((port (open-output-string))
       (st (make-standard-terminal :output-port port))
       (screen (make-main-screen st :auto-hide-cursor #t))
      ) ;
  ;; 渲染第 1 帧
  (screen-render! screen '("frame 1"))
  (let ((out1 (get-output-string port)))
    ;; 第 1 帧应包含隐藏光标 \x1b;[?25l
    (check (string-contains? out1 "\x1b;[?25l") => #t)
    ;; 包含同步输出包裹
    (check (string-contains? out1 "\x1b;[?2026h") => #t)
    (check (string-contains? out1 "\x1b;[?2026l") => #t)
    ;; 清空端口缓冲
    (let* ((port2 (open-output-string))
           (st2 (make-standard-terminal :output-port port2))
           (screen2 (make-main-screen st2 :auto-hide-cursor #t))
          ) ;
      (screen-render! screen2 '("frame 1"))
      (let ((len1 (string-length (get-output-string port2))))
        ;; 渲染第 2 帧（光标已是隐藏状态）
        (screen-render! screen2 '("frame 2"))
        (let* ((full-out (get-output-string port2))
               (frame2-out (substring full-out len1 (string-length full-out)))
              ) ;
          ;; 第 2 帧绝不重复发送 \x1b;[?25l
          (check (string-contains? frame2-out "\x1b;[?25l") => #f)
          ;; 第 2 帧不包含整行清空 \x1b;[2K\r
          (check (string-contains? frame2-out "\x1b;[2K\r") => #f)
        ) ;let*
      ) ;let
    ) ;let*
  ) ;let
) ;let*

;; 5. 逐写入重绘模拟器（Step-by-step repaint simulator）
;; 在每一次 terminal write/clear 操作瞬间对屏幕行进行快照，
;; 验证被更新的那一行在任何瞬时中间态都绝不为空白行（全空格行）。
(let* ((vt (make-virtual-terminal :width 20 :height 3))
       (screen (make-main-screen vt))
       (target-row-snapshots '())
      ) ;
  ;; 初始帧：第 1 行包含内容
  (screen-render! screen '("header" "Heartbeat: ok" "footer"))
  (check (list-ref (virtual-terminal-screen vt) 1) => "Heartbeat: ok       ")

  ;; 自定义包装终端：在底层 write 与 clear 执行时立即捕获屏幕第 1 行
  (let ((wrapped-term
          (make-terminal-record 'virtual
            (lambda (t) (terminal-size vt))
            (lambda (t text)
              (terminal-write vt text)
              (set! target-row-snapshots
                (cons (list-ref (virtual-terminal-screen vt) 1) target-row-snapshots)
              ) ;set!
            ) ;lambda
            (lambda (t) (terminal-cursor vt))
            (lambda (t x y) (terminal-set-cursor! vt x y))
            (lambda (t) (terminal-clear vt))
            (lambda (t)
              (terminal-clear-line vt)
              (set! target-row-snapshots
                (cons (list-ref (virtual-terminal-screen vt) 1) target-row-snapshots)
              ) ;set!
            ) ;lambda
            (lambda (t)
              (terminal-clear-line-to-end vt)
              (set! target-row-snapshots
                (cons (list-ref (virtual-terminal-screen vt) 1) target-row-snapshots)
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
    (let ((screen-wrapped (make-main-screen wrapped-term)))
      ;; 同步上一帧 buffer 到新屏幕
      (screen-buffer-commit! (screen-buffer screen-wrapped)
        '("header" "Heartbeat: ok" "footer")
      ) ;screen-buffer-commit!
      ;; 触发第 2 帧更新（模拟 Heartbeat 动态变化）
      (screen-render! screen-wrapped '("header" "Heartbeat: pulsing" "footer"))

      ;; 检查所有中间快照：没有任何一次快照中第 1 行是全空白！
      (let ((blank-row (make-string 20 #\space)))
        (for-each (lambda (snap) (check (string=? snap blank-row) => #f))
          target-row-snapshots
        ) ;for-each
      ) ;let
    ) ;let

  ) ;let
) ;let*

(check-report)
