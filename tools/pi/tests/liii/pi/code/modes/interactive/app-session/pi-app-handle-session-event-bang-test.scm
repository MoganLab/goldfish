;; pi-app-handle-session-event!
;; 处理后台返回的会话统计事件（展示统计卡片或错误卡片）。
;;
;; 语法
;; ----
;; (pi-app-handle-session-event! app ev)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;; ev : list?
;; 统计事件元组，如 (session_stats stats) 或 (session_stats_error err)。
;;
;; 返回值
;; ----
;; unspecified

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
  (liii pi code modes interactive app-session)
) ;import

(check-set-mode! 'report-failed)

;; 1. session_stats_error 添加错误卡片
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm))
      ) ;
  (pi-app-handle-session-event! app '(session_stats_error "Not found"))
  (check
    (> (length (container-children (%pi-app-chat app))) 0)
    =>
    #t
  ) ;check
) ;let*

(check-report)
