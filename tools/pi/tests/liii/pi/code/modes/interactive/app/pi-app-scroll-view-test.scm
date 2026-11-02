;; pi-app-scroll-view
;; 获取应用关联的对话记录滚动视口组件。
;;
;; 语法
;; ----
;; (pi-app-scroll-view app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ------
;; any
;; 对话记录视口组件对象。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 应用实例包含非空滚动视口
(let* ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24)))
       (sv (pi-app-scroll-view app))
      ) ;
  (check-true (scroll-view? sv))
) ;let*

(check-report)
