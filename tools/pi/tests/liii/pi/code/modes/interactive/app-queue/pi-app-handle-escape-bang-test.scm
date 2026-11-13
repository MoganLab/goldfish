;; pi-app-handle-escape!
;; 处理 ESC 快捷键：若有激活选区则清除选区；否则向后台任务发送中止信号、标记 cancelling 并将排队消息撤回至输入框。
;;
;; 语法
;; ----
;; (pi-app-handle-escape! app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ----
;; boolean?
;; 恒返回 #t。

(import (scheme base)
  (liii check)
  (liii go)
  (liii tui)
  (liii pi agent queue)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
  (liii pi code modes interactive app-queue)
) ;import

(check-set-mode! 'report-failed)

;; 1. busy 状态下按 ESC：发出 abort 信号、转入 cancelling 状态并撤回排队消息到编辑器
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (q (pi-app-queue app))
       (ed (pi-app-editor app))
      ) ;
  (pi-queue-set-busy! q)
  (pi-app-queue-steering! app "排队消息 1")
  (editor-set-text! ed "未发草稿")
  (check (pi-app-handle-escape! app) => #t)
  ;; 验证：向 abort-chan 发送了 abort 信号
  (check (chan-try-recv! (pi-app-abort-chan app) #f) => 'abort)
  ;; 验证：队列状态转入 cancelling
  (check-true (pi-queue-cancelling? q))
  (check-false (pi-queue-busy? q))
  ;; 验证：排队消息撤回并与草稿合并
  (check (%pi-app-steering-pending app) => '())
  (check (editor-text ed) => "排队消息 1\n\n未发草稿")
) ;let*

;; 2. idle 状态下按 ESC：不发送 abort 信号，队列保持 idle
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (q (pi-app-queue app))
      ) ;
  (check (pi-app-handle-escape! app) => #t)
  (check (chan-try-recv! (pi-app-abort-chan app) #f) => #f)
  (check-true (pi-queue-idle? q))
) ;let*

(check-report)
