;; %pi-app-scroll-hint-rect-set!
;; 设置应用实例当前渲染帧的“跳到最新消息”提示热区矩形。
;;
;; 语法
;; ----
;; (%pi-app-scroll-hint-rect-set! app rect)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; rect : list? 或 #f
;; 提示热区坐标 (row col width)，或 #f 表示无有效热区。
;;
;; 返回值
;; ----
;; unspecified
;; 返回值未指定。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
) ;import

(check-set-mode! 'report-failed)

;; 1. 写入列表后可读回
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (%pi-app-scroll-hint-rect-set! app '(18 21 37))
  (check (pi-app-scroll-hint-rect app) => '(18 21 37))
) ;let

;; 2. 覆盖写入与清空为 #f
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (%pi-app-scroll-hint-rect-set! app '(24 31 37))
  (check (pi-app-scroll-hint-rect app) => '(24 31 37))
  (%pi-app-scroll-hint-rect-set! app #f)
  (check (pi-app-scroll-hint-rect app) => #f)
) ;let

(check-report)
