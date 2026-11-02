;; pi-app-scroll-bottom!
;; 交互式应用对话记录视口滚动至底部并恢复跟随最新消息模式。
;;
;; 语法
;; ----
;; (pi-app-scroll-bottom! app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ------
;; any
;; 执行视口滚动至底部的副作用。

(import (scheme base)
  (liii check)
  (liii string)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 滚动至底部后恢复 follow 模式
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (let loop
    ((i 0))
    (when (< i 30)
      (pi-app-add-user-message! app (string-append "msg-" (number->string i)))
      (loop (+ i 1))
    ) ;when
  ) ;let
  (pi-app-scroll-top! app)
  (check-false (scroll-view-follow? (pi-app-scroll-view app)))
  (pi-app-scroll-bottom! app)
  (check-true (scroll-view-follow? (pi-app-scroll-view app)))
) ;let

(check-report)
