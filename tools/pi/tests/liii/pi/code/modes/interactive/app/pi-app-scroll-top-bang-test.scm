;; pi-app-scroll-top!
;; 交互式应用对话记录视口滚动至最顶部。
;;
;; 语法
;; ----
;; (pi-app-scroll-top! app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ------
;; any
;; 执行视口滚动至顶部的副作用。

(import (scheme base)
  (liii check)
  (liii string)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 滚动至顶部后 offset 为 0 且 follow 为 #f
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (let loop
    ((i 0))
    (when (< i 30)
      (pi-app-add-user-message! app (string-append "msg-" (number->string i)))
      (loop (+ i 1))
    ) ;when
  ) ;let
  (pi-app-scroll-top! app)
  (check (scroll-view-offset (pi-app-scroll-view app)) => 0)
  (check-false (scroll-view-follow? (pi-app-scroll-view app)))
) ;let

(check-report)
