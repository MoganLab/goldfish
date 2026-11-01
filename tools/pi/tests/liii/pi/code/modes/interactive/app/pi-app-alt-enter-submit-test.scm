(import (scheme base)
  (liii list)
  (liii string)
  (liii check)
  (liii go)
  (liii tui)
  (liii pi agent queue)
  (liii pi code modes interactive)
  (liii pi code modes interactive app)
  (liii pi code modes interactive run)
) ;import

(check-set-mode! 'report-failed)

;; -----------------------------------------------------------------------------
;; 测试项 1: busy 时 alt+enter 提交 → follow-up 注入路径，编辑器清空并入历史
;; -----------------------------------------------------------------------------
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (ed (pi-app-editor app))
      ) ;
  (pi-queue-set-busy! (pi-app-queue app))
  (editor-set-text! ed "等结束后总结")
  (check (pi-app-handle-input! app 'alt-enter) => #t)
  ;; follow-up 显示列表与通道就位；steering 不受影响
  (check (%pi-app-follow-up-pending app) => '("等结束后总结"))
  (check (%pi-app-steering-pending app) => '())
  (check (chan-try-recv! (pi-app-follow-up-chan app) #f) => "等结束后总结")
  ;; 编辑器清空，且消息进入 ↑ 历史
  (check (editor-text ed) => "")
  (check (editor-history-list ed) => '("等结束后总结"))
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 2: busy 时普通回车仍是 steering；alt+enter 不改 steering 队列
;; -----------------------------------------------------------------------------
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (ed (pi-app-editor app))
      ) ;
  (pi-queue-set-busy! (pi-app-queue app))
  ;; 普通回车（经编辑器 on-submit）
  (editor-set-text! ed "插话")
  (check (pi-app-handle-input! app 'enter) => #t)
  (check (%pi-app-steering-pending app) => '("插话"))
  ;; alt+enter
  (editor-set-text! ed "排后面")
  (check (pi-app-handle-input! app 'alt-enter) => #t)
  (check (%pi-app-steering-pending app) => '("插话"))
  (check (%pi-app-follow-up-pending app) => '("排后面"))
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 3: 空闲时 alt+enter 等同普通提交（直接派发新任务）
;; -----------------------------------------------------------------------------
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (processed '())
       (app
         (make-pi-app
           :terminal vt
           :handler (lambda (a m) (set! processed (cons m processed)))
         ) ;make-pi-app
       ) ;app
       (ed (pi-app-editor app))
      ) ;
  (editor-set-text! ed "新任务")
  (check (pi-app-handle-input! app 'alt-enter) => #t)
  (check processed => '("新任务"))
  (check (pi-queue-idle? (pi-app-queue app)) => #t)
  (check (%pi-app-follow-up-pending app) => '())
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 4: 编辑器为空时 alt+enter 无操作
;; -----------------------------------------------------------------------------
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
      ) ;
  (pi-queue-set-busy! (pi-app-queue app))
  (check (pi-app-handle-input! app 'alt-enter) => #t)
  (check (%pi-app-follow-up-pending app) => '())
  (check (pi-queue-length (pi-app-queue app)) => 0)
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 5: 主循环 alt+up 仅撤回不中止：steering/follow-up 合并回编辑器，
;; 不发送 abort 信号
;; -----------------------------------------------------------------------------
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (ed (pi-app-editor app))
       (q (pi-app-queue app))
      ) ;
  (pi-queue-set-busy! q)
  (pi-app-submit! app "插话 A")
  (pi-app-submit! app "排后面 B" 'follow-up)
  (editor-set-text! ed "草稿")
  (virtual-terminal-push-key! vt 'alt-up)
  (virtual-terminal-push-key! vt 'ctrl-c) ;; 清掉合并后的文本，便于 ctrl-d 退出
  (virtual-terminal-push-key! vt 'ctrl-d)
  (check (pi-app-run app) => #t)
  ;; 撤回合并：steering → follow-up → 编辑器草稿
  ;; （ctrl-c 已清空编辑器，这里通过渲染前状态验证无法直接取文本，
  ;;   改验队列已清空且未发 abort）
  (check (%pi-app-steering-pending app) => '())
  (check (%pi-app-follow-up-pending app) => '())
  (check (pi-queue-busy? q) => #t)
  ;; alt+up 未发送 abort 信号（通道应保持空）
  (check (chan-try-recv! (pi-app-abort-chan app) #f) => #f)
  (pi-queue-finish-task! q)
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 6: 主循环 ESC 中止并撤回：steering 显示清空、abort 信号已发出
;; -----------------------------------------------------------------------------
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (ed (pi-app-editor app))
       (q (pi-app-queue app))
      ) ;
  (pi-queue-set-busy! q)
  (pi-app-submit! app "插话 C")
  (virtual-terminal-push-key! vt 'escape)
  (virtual-terminal-push-key! vt 'ctrl-c)
  (virtual-terminal-push-key! vt 'ctrl-d)
  (check (pi-app-run app) => #t)
  ;; ESC 发出了 abort 信号
  (check (chan-try-recv! (pi-app-abort-chan app) #f) => 'abort)
  (check (%pi-app-steering-pending app) => '())
  (pi-queue-finish-task! q)
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 7: shift+enter 在 pi 输入框中插入换行，不触发提交
;; -----------------------------------------------------------------------------
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (submitted #f)
       (app
         (make-pi-app
           :terminal vt
           :handler (lambda (a m) (set! submitted m))
         ) ;make-pi-app
       ) ;app
       (ed (pi-app-editor app))
      ) ;
  (editor-set-text! ed "第一行")
  (check (pi-app-handle-input! app 'shift-enter) => #t)
  (check (editor-text ed) => "第一行\n")
  (check (editor-cursor ed) => '(1 0))
  (check submitted => #f)
) ;let*

(check-report)
