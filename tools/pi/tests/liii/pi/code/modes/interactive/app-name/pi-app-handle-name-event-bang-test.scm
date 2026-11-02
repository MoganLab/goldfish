;; pi-app-handle-name-event!
;; 处理后台返回的会话命名相关事件（更新会话名或报错）。
;;
;; 语法
;; ----
;; (pi-app-handle-name-event! app ev)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;; ev : list?
;; 命名事件，如 (session_name_changed name) 或 (session_name_error err)。
;;
;; 返回值
;; ----
;; unspecified

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
  (liii pi code modes interactive app-name)
) ;import

(check-set-mode! 'report-failed)

;; 1. session_name_changed 更新应用会话名
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm))
      ) ;
  (pi-app-handle-name-event! app '(session_name_changed "NewSession"))
  (check (pi-app-session-name app) => "NewSession")
) ;let*

;; 2. session_name_error 添加错误卡片
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm))
      ) ;
  (pi-app-handle-name-event! app '(session_name_error "Disk full"))
  (check
    (> (length (container-children (%pi-app-chat app))) 0)
    =>
    #t
  ) ;check
) ;let*

(check-report)
