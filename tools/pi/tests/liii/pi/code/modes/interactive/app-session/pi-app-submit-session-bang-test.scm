;; pi-app-submit-session!
;; 提交 /session 命令，向 worker 投递统计请求并清空输入。
;;
;; 语法
;; ----
;; (pi-app-submit-session! app)
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
  (liii pi code modes interactive app-session)
) ;import

(check-set-mode! 'report-failed)

;; 1. 执行命令后清空输入并投递统计任务
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm))
      ) ;
  (editor-set-text! (pi-app-editor app) "/session")
  (pi-app-submit-session! app)
  (check (editor-text (pi-app-editor app)) => "")
) ;let*

(check-report)
