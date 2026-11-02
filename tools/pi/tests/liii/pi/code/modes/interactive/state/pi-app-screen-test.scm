;; pi-app-screen
;; 获取应用实例关联的备用屏幕缓冲区。
;;
;; 语法
;; ----
;; (pi-app-screen app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ------
;; any
;; 屏幕缓冲区对象。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 验证获取非空屏幕缓冲区
(let* ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24)))
       (screen (pi-app-screen app))
      ) ;
  (check-true (alt-screen? screen))
) ;let*

(check-report)
