;; %pi-app-editor-on-change
;; 编辑器内容变化回调：识别斜杠菜单与 Bash 模式，并更新输入边框样式。
;;
;; 语法
;; ----
;; (%pi-app-editor-on-change app ed text)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;; ed : editor?
;; 编辑器组件。
;; text : string?
;; 当前编辑器输入文本。
;;
;; 返回值
;; ----
;; unspecified

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
  (liii pi code modes interactive app-submit)
) ;import

(check-set-mode! 'report-failed)

;; 1. 输入 ! 开头的命令进入 bash 模式
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm))
       (ed (pi-app-editor app))
      ) ;
  (%pi-app-editor-on-change app ed "!ls -la")
  (check-true (pi-app-bash-mode? app))

  ;; 2. 输入普通文本退出 bash 模式
  (%pi-app-editor-on-change app ed "hello world")
  (check-false (pi-app-bash-mode? app))
) ;let*

(check-report)
