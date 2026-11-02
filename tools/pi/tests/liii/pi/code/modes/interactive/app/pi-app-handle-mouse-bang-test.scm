;; pi-app-handle-mouse!
;; 处理单次鼠标事件（滚轮滚动视口或点击交互卡片）。
;;
;; 语法
;; ----
;; (pi-app-handle-mouse! app event)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;; event : mouse-event?
;; 待处理的鼠标事件对象。
;;
;; 返回值
;; ------
;; boolean
;; 事件被消费且需要重绘界面时返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii string)
  (liii tui)
  (liii tui mouse)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 滚轮事件触发处理并返回 #t
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  ;; 添加多条消息以填充视口
  (let loop
    ((i 0))
    (when (< i 20)
      (pi-app-add-user-message! app (string-append "msg-" (number->string i)))
      (loop (+ i 1))
    ) ;when
  ) ;let
  (check-true (pi-app-handle-mouse! app (make-mouse-event 'wheel-up 'none 10 5)))
  (check-true (pi-app-handle-mouse! app (make-mouse-event 'wheel-down 'none 10 5))
  ) ;check-true
) ;let

(check-report)
