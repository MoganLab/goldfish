;; pi-app-scroll-hint-rect
;; 获取应用实例当前渲染帧的“跳到最新消息”提示热区矩形。
;;
;; 语法
;; ----
;; (pi-app-scroll-hint-rect app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ----
;; list? 或 #f
;; 当前提示热区坐标 (row col width)；处于跟随末尾状态或无提示时为 #f。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
) ;import

(check-set-mode! 'report-failed)

;; 1. 初始状态：无热区，返回 #f
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (check (pi-app-scroll-hint-rect app) => #f)
) ;let

;; 2. 写入热区后读取
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (%pi-app-scroll-hint-rect-set! app '(18 21 37))
  (check (pi-app-scroll-hint-rect app) => '(18 21 37))
) ;let

(check-report)
