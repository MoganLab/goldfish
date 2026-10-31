(import (scheme base)
  (liii list)
  (liii string)
  (liii check)
  (liii go)
  (liii tui)
  (liii pi agent queue)
  (liii pi code modes interactive app)
  (liii pi code modes interactive run)
) ;import

(check-set-mode! 'report)

;; 语义约定：
;;   Ctrl+C —— 仅清空输入框，永不退出、不中断任务。
;;   Ctrl+D —— 输入框严格为空时退出（EOF 语义）；非空时前向删除
;;             光标右侧字符（与 Delete 键一致），删除后变空也不退出。
;;   Escape —— busy 时发 abort 信号中断任务（对齐上游 pi），并把排队
;;             消息撤回至 editor 重新编辑；不退出 TUI。
;; 因此所有「需要退出主循环」的测试都必须用 Ctrl+D 触发，
;; 且退出前必须保证输入框为空（必要时先按 Ctrl+C 清空）。
;; Ctrl+D 的删除语义（英文/中文/多行/菜单/撤销等）详见
;; pi-app-run-ctrl-d-test.scm。

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

;; 6. 队列空闲时按 escape 不发 abort 信号（保持现有行为）
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (q (pi-app-queue app))
      ) ;
  (check-true (pi-queue-idle? q))
  (virtual-terminal-push-key! vt 'escape)
  ;; 输入框为空，Ctrl+D 直接退出
  (virtual-terminal-push-key! vt 'ctrl-d)
  (check (pi-app-run app) => #t)
  (check (chan-try-recv! (pi-app-abort-chan app) #f) => #f)
) ;let*

;; 7. busy 时按 escape：发送 abort 信号并撤回排队消息到编辑器
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (q (pi-app-queue app))
       (ed (pi-app-editor app))
      ) ;
  (pi-queue-enqueue! q "queued-1")
  (pi-queue-enqueue! q "queued-2")
  (pi-queue-set-busy! q)
  (virtual-terminal-push-key! vt 'escape)
  ;; 此时编辑器已恢复 "queued-1\nqueued-2"：先按一次 Ctrl+D 证明
  ;; 有内容时不退出（此时删除光标处字符），再用 Ctrl+C 清空后 Ctrl+D 退出
  (virtual-terminal-push-key! vt 'ctrl-d)
  (virtual-terminal-push-key! vt 'ctrl-c)
  (virtual-terminal-push-key! vt 'ctrl-d)
  (check (pi-app-run app) => #t)
  ;; abort 信号已发送到 abort-chan
  (check (chan-try-recv! (pi-app-abort-chan app) #f) => 'abort)
  ;; 排队消息已撤回（队列清空，消息经编辑器又被 Ctrl+C 清空）
  (check (pi-queue-length q) => 0)
  (check (editor-text ed) => "")
) ;let*

;; 8. busy 且队列为空时按 escape：仍发送 abort 信号（中断正在进行的任务）
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (q (pi-app-queue app))
       (ed (pi-app-editor app))
      ) ;
  (pi-queue-set-busy! q)
  (virtual-terminal-push-key! vt 'escape)
  ;; 编辑器为空，Ctrl+D 直接退出
  (virtual-terminal-push-key! vt 'ctrl-d)
  (check (pi-app-run app) => #t)
  (check (chan-try-recv! (pi-app-abort-chan app) #f) => 'abort)
  (check (editor-text ed) => "")
) ;let*

;; 9. Ctrl+C 在 busy 时也不中断任务、不退出，也不追加任何卡片
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
  (check (> (length (alt-screen-lines screen)) 0) => #t)
) ;let*

;; 10. Ctrl+D 在输入框有内容时不退出（避免误触丢草稿）
;;     光标位于缓冲区末尾，此时没有可删除的内容，Ctrl+D 为无操作
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (ed (pi-app-editor app))
      ) ;
  (pi-app-render! app)
  (component-handle-input ed "草稿")
  (virtual-terminal-push-key! vt 'ctrl-d)
  ;; 无可删除内容时 Ctrl+D 应无效：再输入 "z" 应追加到草稿后，证明仍在主循环
  (virtual-terminal-push-key! vt "z")
  (virtual-terminal-push-key! vt 'ctrl-c)
  (virtual-terminal-push-key! vt 'ctrl-d)
  (check (pi-app-run app) => #t)
  ;; Ctrl+D 未生效且 "z" 被追加，随后 Ctrl+C 清空
  (check (editor-text ed) => "")
) ;let*

;; 11. agent_settled 带 aborted=#t 时上屏 Aborted 提示卡片并结算队列
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (q (pi-app-queue app))
      ) ;
  (pi-queue-set-busy! q)
  (chan-send! (pi-app-event-chan app) '(agent_settled #t) 1000)
  ;; 输入框为空，Ctrl+D 直接退出
  (virtual-terminal-push-key! vt 'ctrl-d)
  (check (pi-app-run app) => #t)
  ;; 队列回到 idle
  (check-true (pi-queue-idle? q))
  ;; Aborted 提示卡片已渲染上屏
  (let* ((lines (alt-screen-lines (pi-app-screen app)))
         (plain-lines (map strip-ansi lines))
        ) ;
    (check-true
      (pair? (filter (lambda (l) (string-contains? l "Aborted")) plain-lines))
    ) ;check-true
  ) ;let*
) ;let*

;; 12. 全屏模式：进入时切换到备用屏，退出时离开备用屏并把对话记录打印回主屏
;;     （对齐上游 fullscreenExitOutput=transcript 行为）
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (screen (pi-app-screen app))
      ) ;
  (pi-app-add-user-message! app "hello fullscreen")
  (virtual-terminal-push-key! vt 'ctrl-d)
  (pi-app-run app)
  (let* ((raw (string-join (virtual-terminal-raw vt) ""))
         (lines (alt-screen-lines screen)))
    (check (> (length lines) 0) => #t)
    ;; 进入与退出备用屏的序列都已下发
    (check-true (string-contains? raw "\x1b;[?1049h"))
    (check-true (string-contains? raw "\x1b;[?1049l"))
  ) ;let*
) ;let*

;; 13. TUI 启动时依次提交 CLI initial-msg 与 rest-msgs，再进入交互主循环
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (received '())
       (app
         (make-pi-app
           :terminal vt
           :handler  (lambda (a msg) (set! received (cons msg received)))
         ) ;make-pi-app
       ) ;app
      ) ;
  (virtual-terminal-push-key! vt 'ctrl-d)
  (check (pi-app-run app "I am a frog" '("who am I")) => #t)
  (check (reverse received) => '("I am a frog" "who am I"))
) ;let*

;; 14. ThinkingSelector 激活时 Ctrl+C 取消选择器而不是清空主编辑器；随后 Ctrl+D 正常退出
(let* ((vt (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t) :thinking-level "medium")
       ) ;app
      ) ;
  (pi-app-submit! app "/thinking")
  (check-true (component? (pi-app-active-selector app)))
  (virtual-terminal-push-key! vt 'ctrl-c)
  (virtual-terminal-push-key! vt 'ctrl-d)
  (check (pi-app-run app) => #t)
  (check (pi-app-active-selector app) => #f)
  (check (pi-app-thinking-level app) => "medium")
) ;let*

;; 15. 命令菜单激活时 Escape 仅关闭菜单；随后 Enter 把保留的 / 作为普通输入提交。
(let* ((vt (make-virtual-terminal :width 100 :height 40))
       (received '())
       (app
         (make-pi-app
           :terminal vt
           :handler  (lambda (a msg) (set! received (cons msg received)))
         ) ;make-pi-app
       ) ;app
      ) ;
  (virtual-terminal-push-key! vt "/")
  (virtual-terminal-push-key! vt 'escape)
  (virtual-terminal-push-key! vt 'enter)
  (virtual-terminal-push-key! vt 'ctrl-c)
  (virtual-terminal-push-key! vt 'ctrl-d)
  (check (pi-app-run app) => #t)
  (check received => '("/"))
  (check (pi-app-active-selector app) => #f)
) ;let*

;; 16. Ctrl+Z 撤销：主循环把未消费按键交给编辑器，撤销后再提交，
;;     提交内容不含已被撤销的文本（若撤销失效会提交 "abcd"）
(let* ((submitted '())
       (vt (make-virtual-terminal :width 80 :height 24))
       (app
         (make-pi-app
           :terminal vt
           :handler  (lambda (a msg) (set! submitted (cons msg submitted)))
         ) ;make-pi-app
       ) ;app
       (ed (pi-app-editor app))
      ) ;
  (component-handle-input ed "ab")
  (virtual-terminal-push-key! vt 'ctrl-z)
  (virtual-terminal-push-key! vt "cd")
  (virtual-terminal-push-key! vt 'enter)
  (virtual-terminal-push-key! vt 'ctrl-d)
  (check (pi-app-run app) => #t)
  (check submitted => '("cd"))
  (check (editor-text ed) => "")
  (check (virtual-terminal-pop-key! vt) => #f)
) ;let*

;; N+1. Ctrl+T 全局切换思考块显隐（对齐上游 app.thinking.toggle）：
;;      idle + 空输入框时触发（编辑器 transpose 未消费后上抛），
;;      TUI 不崩溃，hide-thinking 翻转为 #t，再按一次翻回 #f
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (ed (pi-app-editor app))
      ) ;
  (check (pi-app-hide-thinking? app) => #f)
  (virtual-terminal-push-key! vt 'ctrl-t)
  (virtual-terminal-push-key! vt 'ctrl-t)
  (virtual-terminal-push-key! vt 'ctrl-d)
  (check (pi-app-run app) => #t)
  (check (pi-app-hide-thinking? app) => #f)
) ;let*

(check-report)
