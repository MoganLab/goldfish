(import (scheme base)
  (liii list)
  (liii string)
  (liii check)
  (liii go)
  (liii tui)
  (liii pi agent queue)
  (liii pi code tui app)
  (liii pi code tui run)
) ;import

(check-set-mode! 'report)

;; 1. 测试 pi-app-run 过程定义
(check-true (procedure? pi-app-run))

;; 2. 测试 pi-app-run 在虚拟终端中接收到 ctrl-c 时正常退出
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
      ) ;
  (virtual-terminal-push-key! vt 'ctrl-c)
  (check (pi-app-run app) => #t)
) ;let*

;; 3. 测试队列空闲时按 escape 不会退出 TUI（后续 ctrl-c 才退出）
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
      ) ;
  (virtual-terminal-push-key! vt 'escape)
  (virtual-terminal-push-key! vt 'ctrl-c)
  (check (pi-app-run app) => #t)
  ;; 若 escape 误退出 TUI，ctrl-c 将残留队列中未被消费
  (check (virtual-terminal-pop-key! vt) => #f)
) ;let*

;; 4. 队列空闲时按 escape 不发 abort 信号（保持现有行为）
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (q (pi-app-queue app))
      ) ;
  (check-true (pi-queue-idle? q))
  (virtual-terminal-push-key! vt 'escape)
  (virtual-terminal-push-key! vt 'ctrl-c)
  (check (pi-app-run app) => #t)
  (check (chan-try-recv! (pi-app-abort-chan app) #f) => #f)
) ;let*

;; 5. busy 时按 escape：发送 abort 信号并撤回排队消息到编辑器
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (q (pi-app-queue app))
       (ed (pi-app-editor app))
      ) ;
  (pi-queue-enqueue! q "queued-1")
  (pi-queue-enqueue! q "queued-2")
  (pi-queue-set-busy! q)
  (virtual-terminal-push-key! vt 'escape)
  (virtual-terminal-push-key! vt 'ctrl-c)
  (check (pi-app-run app) => #t)
  ;; abort 信号已发送到 abort-chan
  (check (chan-try-recv! (pi-app-abort-chan app) #f) => 'abort)
  ;; 排队消息已恢复到编辑器
  (check (editor-text ed) => "queued-1\nqueued-2")
  ;; 队列已清空
  (check (pi-queue-length q) => 0)
) ;let*

;; 6. busy 且队列为空时按 escape：仍发送 abort 信号（中断正在进行的任务）
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (q (pi-app-queue app))
       (ed (pi-app-editor app))
      ) ;
  (pi-queue-set-busy! q)
  (virtual-terminal-push-key! vt 'escape)
  (virtual-terminal-push-key! vt 'ctrl-c)
  (check (pi-app-run app) => #t)
  (check (chan-try-recv! (pi-app-abort-chan app) #f) => 'abort)
  (check (editor-text ed) => "")
) ;let*

;; 7. agent_settled 带 aborted=#t 时上屏 Aborted 提示卡片并结算队列
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (q (pi-app-queue app))
      ) ;
  (pi-queue-set-busy! q)
  (chan-send! (pi-app-event-chan app) '(agent_settled #t) 1000)
  (virtual-terminal-push-key! vt 'ctrl-c)
  (check (pi-app-run app) => #t)
  ;; 队列回到 idle
  (check-true (pi-queue-idle? q))
  ;; Aborted 提示卡片已渲染上屏
  (let* ((lines (screen-lines (pi-app-screen app)))
         (plain-lines (map strip-ansi lines))
        ) ;
    (check-true
      (pair?
        (filter
          (lambda (l) (string-contains? l "Aborted"))
          plain-lines
        ) ;filter
      ) ;pair?
    ) ;check-true
  ) ;let*
) ;let*

(check-report)
