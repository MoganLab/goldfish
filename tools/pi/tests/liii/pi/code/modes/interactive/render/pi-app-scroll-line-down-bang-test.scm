;; pi-app-scroll-line-down!
;; 交互式应用对话记录视口向下滚动一行。
;;
;; 语法
;; ----
;; (pi-app-scroll-line-down! app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ------
;; any
;; 执行视口向下滚动一行的副作用。

(import (scheme base)
  (liii check)
  (liii string)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 执行单行向下滚动
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (let loop
    ((i 0))
    (when (< i 30)
      (pi-app-add-user-message! app (string-append "msg-" (number->string i)))
      (loop (+ i 1))
    ) ;when
  ) ;let
  (pi-app-scroll-top! app)
  (let ((before (scroll-view-offset (pi-app-scroll-view app))))
    (pi-app-scroll-line-down! app)
    (check-true (> (scroll-view-offset (pi-app-scroll-view app)) before))
  ) ;let
) ;let

(check-report)
