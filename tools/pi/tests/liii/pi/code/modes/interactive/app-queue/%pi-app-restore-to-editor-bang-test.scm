;; %pi-app-restore-to-editor!
;; 将待处理的排队消息（steering / follow-up / 等待队列）撤回并与输入框草稿合并。
;;
;; 语法
;; ----
;; (%pi-app-restore-to-editor! app)
;;
;; 参数
;; ----
;; app : pi-app?，交互式应用实例。
;;
;; 返回值
;; ----
;; unspecified

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi agent queue)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
  (liii pi code modes interactive app-queue)
) ;import

(check-set-mode! 'report-failed)

;; 1. 撤回排队消息并与编辑器既有草稿以换行合并
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (ed (pi-app-editor app))
      ) ;
  (pi-app-queue-steering! app "排队 1")
  (pi-app-queue-follow-up! app "排队 2")
  (editor-set-text! ed "既有草稿")
  (%pi-app-restore-to-editor! app)
  (check (editor-text ed) => "排队 1\n\n排队 2\n\n既有草稿")
  (check (%pi-app-steering-pending app) => '())
  (check (%pi-app-follow-up-pending app) => '())
) ;let*

;; 2. 无排队消息时保持编辑器草稿不变
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (ed (pi-app-editor app))
      ) ;
  (editor-set-text! ed "单行草稿")
  (%pi-app-restore-to-editor! app)
  (check (editor-text ed) => "单行草稿")
) ;let*

(check-report)
