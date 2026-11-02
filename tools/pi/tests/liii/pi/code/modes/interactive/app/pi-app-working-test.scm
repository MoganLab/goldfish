;; pi-app-working
;; 获取当前应用实例关联的 Working 动画指示器。
;;
;; 语法
;; ----
;; (pi-app-working app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ------
;; any
;; Working 动画指示器对象。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 应用实例包含非空 Working 指示器
(let* ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24)))
       (w (pi-app-working app))
      ) ;
  (check-true (and w #t))
) ;let*

(check-report)
