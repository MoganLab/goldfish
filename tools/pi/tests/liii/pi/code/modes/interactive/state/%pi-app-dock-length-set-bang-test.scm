;; %pi-app-dock-length-set!
;; 设置应用实例记录的底部 dock 行数（待发送区 + 输入区 + 菜单 + 页脚）。
;;
;; 语法
;; ----
;; (%pi-app-dock-length-set! app length)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; length : exact-integer?
;; 新的 dock 行数。
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

;; 1. 写入后可读回
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (%pi-app-dock-length-set! app 7)
  (check (pi-app-dock-length app) => 7)
) ;let

;; 2. 覆盖写
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (%pi-app-dock-length-set! app 3)
  (%pi-app-dock-length-set! app 5)
  (check (pi-app-dock-length app) => 5)
) ;let

(check-report)
