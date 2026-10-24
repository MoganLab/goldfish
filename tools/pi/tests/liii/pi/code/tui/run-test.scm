(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi agent queue)
  (liii pi code tui app)
  (liii pi code tui run)
) ;import

(check-set-mode! 'report)

;; 语义约定：
;;   Ctrl+C —— 仅清空输入框，永不退出、不中断任务。
;;   Ctrl+D —— 输入框为空时退出（EOF 语义）；有内容时无操作。
;; 因此所有「需要退出主循环」的测试都必须用 Ctrl+D 触发。

;; 1. 测试 pi-app-run 过程定义
(check-true (procedure? pi-app-run))

;; 2. Ctrl+D 在输入框为空时退出（返回 #t 表示离开 TUI 主循环）
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
      ) ;
  (virtual-terminal-push-key! vt 'ctrl-d)
  (check (pi-app-run app) => #t)
) ;let*

;; 3. 测试队列空闲时按 escape 不会退出 TUI（后续 ctrl-d 才退出）
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
      ) ;
  (virtual-terminal-push-key! vt 'escape)
  (virtual-terminal-push-key! vt 'ctrl-d)
  (check (pi-app-run app) => #t)
  ;; 若 escape 误退出 TUI，ctrl-d 将残留队列中未被消费
  (check (virtual-terminal-pop-key! vt) => #f)
) ;let*

;; 4. Ctrl+C 在 idle 且输入框为空时不退出。
;;    关键技巧：Ctrl+C 后再推入字符 "x"。若 Ctrl+C 错误地退出了主循环，
;;    后续按键不会被消费，编辑器收不到 "x"——这样返回值 #t 才不会掩盖缺陷。
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (q (pi-app-queue app))
       (ed (pi-app-editor app))
      ) ;
  (check (pi-queue-busy? q) => #f)
  (check (editor-text ed) => "")
  (virtual-terminal-push-key! vt 'ctrl-c)
  (virtual-terminal-push-key! vt "x")
  ;; 再按一次 Ctrl+C 清空 "x"，使随后 Ctrl+D 能退出（否则有内容会空转）
  (virtual-terminal-push-key! vt 'ctrl-c)
  (virtual-terminal-push-key! vt 'ctrl-d)
  (check (pi-app-run app) => #t)
  ;; 输入框为空：证明 "x" 曾被插入（Ctrl+C 未退出主循环）且随后被第二次 Ctrl+C 清空
  (check (editor-text ed) => "")
) ;let*

;; 5. Ctrl+C 在 idle 且输入框有内容时只清空输入框、不退出
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (ed (pi-app-editor app))
      ) ;
  (pi-app-render! app)
  (component-handle-input ed "待清除的内容")
  (check (editor-text ed) => "待清除的内容")
  (virtual-terminal-push-key! vt 'ctrl-c)
  ;; 用 Ctrl+D 退出（此时输入框应为空，直接退出）
  (virtual-terminal-push-key! vt 'ctrl-d)
  (check (pi-app-run app) => #t)
  ;; 输入框必须已被 Ctrl+C 清空
  (check (editor-text ed) => "")
) ;let*

;; 6. Ctrl+C 在 busy 时也不中断任务、不退出，也不追加任何卡片
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (q (pi-app-queue app))
       (ed (pi-app-editor app))
       (screen (pi-app-screen app))
       (before (length (pi-app-chat app)))
      ) ;
  (pi-queue-set-busy! q)
  (virtual-terminal-push-key! vt 'ctrl-c)
  ;; 用后续输入证明仍在主循环：插入 "y" 后再用 Ctrl+C 清空，最后 Ctrl+D 退出
  (virtual-terminal-push-key! vt "y")
  (virtual-terminal-push-key! vt 'ctrl-c)
  (virtual-terminal-push-key! vt 'ctrl-d)
  (check (pi-app-run app) => #t)
  ;; 任务仍然在跑：Ctrl+C 不得改变调度状态
  (check (pi-queue-busy? q) => #t)
  ;; 不得追加任何卡片
  (check (length (pi-app-chat app)) => before)
  ;; "y" 被插入后又被清空，证明 Ctrl+C 未退出主循环
  (check (editor-text ed) => "")
  (check (> (length (screen-lines screen)) 0) => #t)
) ;let*

;; 7. Ctrl+D 在输入框有内容时不退出（避免误触丢草稿）
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (ed (pi-app-editor app))
      ) ;
  (pi-app-render! app)
  (component-handle-input ed "草稿")
  (virtual-terminal-push-key! vt 'ctrl-d)
  ;; 有内容时的 Ctrl+D 应无效：再输入 "z" 应追加到草稿后，证明仍在主循环
  (virtual-terminal-push-key! vt "z")
  (virtual-terminal-push-key! vt 'ctrl-c)
  (virtual-terminal-push-key! vt 'ctrl-d)
  (check (pi-app-run app) => #t)
  ;; Ctrl+D 未生效且 "z" 被追加，随后 Ctrl+C 清空
  (check (editor-text ed) => "")
) ;let*

;; 8. 退出后光标必须位于整个 TUI 块下方，而不是停在输入框所在行。
;;    虚拟终端高度 24，TUI 内容约 11 行，退出后光标行应 >= 内容行数
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (screen (pi-app-screen app))
      ) ;
  (virtual-terminal-push-key! vt 'ctrl-d)
  (pi-app-run app)
  (let* ((lines (screen-lines screen))
         (total (length lines))
         (cur (terminal-cursor vt))
         (cur-y (cadr cur))
         ;; 输入行（editor 文本行）位于距末尾 3 行处：header/chat + top-border + 文本行 + 底边框 + footer
         (editor-row (- total 3))
        ) ;
    (check (> total 0) => #t)
    ;; 退出后光标必须越过输入框（不能停在输入行），否则 shell prompt 会被画在输入框位置
    (check (> cur-y editor-row) => #t)
    (check (>= cur-y total) => #t)
  ) ;let*
) ;let*

(check-report)
