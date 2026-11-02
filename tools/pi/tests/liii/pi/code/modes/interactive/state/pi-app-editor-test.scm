;; pi-app-editor
;; 获取应用实例中关联的输入编辑器组件。
;;
;; 语法
;; ----
;; (pi-app-editor app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ------
;; any
;; 关联的编辑器组件对象。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 验证获取有效的编辑器组件
(let* ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24)))
       (ed (pi-app-editor app))
      ) ;
  (check-true (editor? ed))
) ;let*

(check-report)
