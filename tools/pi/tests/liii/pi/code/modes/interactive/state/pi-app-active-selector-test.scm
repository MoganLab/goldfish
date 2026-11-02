;; pi-app-active-selector
;; 获取当前应用实例激活的模态选择器组件。
;;
;; 语法
;; ----
;; (pi-app-active-selector app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ------
;; any
;; 激活的选择器组件对象，未激活模态选择器时返回 #f。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 默认无激活选择器返回 #f
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (check-false (pi-app-active-selector app))
) ;let

(check-report)
