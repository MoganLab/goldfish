;; pi-app-dock-length
;; 获取应用实例记录的底部 dock 行数（待发送区 + 输入区 + 菜单 + 页脚）。
;;
;; 语法
;; ----
;; (pi-app-dock-length app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ----
;; exact-integer? 或 #f
;; dock 行数；尚未整帧渲染过时为 #f。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
) ;import

(check-set-mode! 'report-failed)

;; 1. 初始状态：尚无整帧渲染，dock 长度未知
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (check (pi-app-dock-length app) => #f)
) ;let

;; 2. 整帧渲染后记录 dock 行数（页脚 + 输入区，至少 2 行）
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
      ) ;
  (pi-app-render! app)
  (check-true (exact-integer? (pi-app-dock-length app)))
  (check-true (> (pi-app-dock-length app) 0))
) ;let*

(check-report)
