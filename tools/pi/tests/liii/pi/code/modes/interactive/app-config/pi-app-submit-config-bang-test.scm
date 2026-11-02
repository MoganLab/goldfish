;; pi-app-submit-config!
;; 处理 /config 命令，在交互界面中嵌套启动配置 TUI 并刷新界面。
;;
;; 语法
;; ----
;; (pi-app-submit-config! app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ----
;; unspecified

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive app-config)
) ;import

(check-set-mode! 'report-failed)

;; 1. 执行 pi-app-submit-config! 会清空编辑器内容，并在按 q 退出配置后恢复渲染
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm))
      ) ;
  (virtual-terminal-push-key! vterm 'q)
  (editor-set-text! (pi-app-editor app) "/config")
  (check (editor-text (pi-app-editor app)) => "/config")
  (pi-app-submit-config! app)
  (check (editor-text (pi-app-editor app)) => "")
) ;let*

(check-report)
