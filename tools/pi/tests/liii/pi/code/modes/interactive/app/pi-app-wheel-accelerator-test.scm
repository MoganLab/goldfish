;; pi-app-wheel-accelerator
;; 获取应用实例关联的鼠标滚轮加速度计算器。
;;
;; 语法
;; ----
;; (pi-app-wheel-accelerator app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ------
;; any
;; 滚轮加速度计对象。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii tui mouse)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 应用实例包含非空滚轮加速度计
(let* ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24)))
       (acc (pi-app-wheel-accelerator app))
      ) ;
  (check-true (wheel-scroll-accelerator? acc))
) ;let*

(check-report)
