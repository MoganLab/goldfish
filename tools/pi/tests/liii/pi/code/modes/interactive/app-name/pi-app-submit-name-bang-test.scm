;; pi-app-submit-name!
;; 提交 /name 命令处理：无参数展示当前名称，有参数投递改名任务。
;;
;; 语法
;; ----
;; (pi-app-submit-name! app text)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;; text : string?
;; 用户输入的完整 /name 命令。
;;
;; 返回值
;; ----
;; unspecified

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive app-name)
) ;import

(check-set-mode! 'report-failed)

;; 1. 无参数时显示当前会话名或用法
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm))
      ) ;
  (pi-app-submit-name! app "/name")
  (check (editor-text (pi-app-editor app)) => "")
) ;let*

;; 2. 有参数时投递命名任务并清空输入
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm))
      ) ;
  (pi-app-submit-name! app "/name test-session")
  (check (editor-text (pi-app-editor app)) => "")
) ;let*

(check-report)
