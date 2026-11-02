;; pi-app-slash-command-menu
;; 获取应用实例中关联的斜杠命令菜单组件。
;;
;; 语法
;; ----
;; (pi-app-slash-command-menu app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ------
;; any
;; 斜杠命令菜单组件对象。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 初始未激活菜单为 #f，输入 / 后激活菜单
(let* ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24)))
       (ed (pi-app-editor app))
      ) ;
  (check-false (pi-app-slash-command-menu app))
  (component-handle-input ed "/")
  (check-true (component? (pi-app-slash-command-menu app)))
) ;let*

(check-report)
