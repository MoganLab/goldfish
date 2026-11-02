;; pi-app-handle-resume-event!
;; 处理后台返回的会话恢复成功或失败事件并更新界面。
;;
;; 语法
;; ----
;; (pi-app-handle-resume-event! app ev)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;; ev : list
;; 会话恢复事件（如 '(session_resume_error "reason")）。
;;
;; 返回值
;; ------
;; any
;; 执行界面提示更新与卡片追加的副作用。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 处理恢复失败事件并追加错误卡片
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm))
      ) ;
  (pi-app-handle-resume-event! app '(session_resume_error "file not found"))
  (check-true (pair? (pi-app-chat app)))
) ;let*

(check-report)
