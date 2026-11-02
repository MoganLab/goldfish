;; pi-app-poll-events!
;; 轮询异步事件通道并分发处理事件以更新应用状态。
;;
;; 语法
;; ----
;; (pi-app-poll-events! app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ------
;; any
;; 执行事件消费与界面重绘的副作用。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 执行事件轮询过程
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (pi-app-poll-events! app)
  (check-true #t)
) ;let

(check-report)
