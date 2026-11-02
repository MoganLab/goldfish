;; pi-app-drain-queue!
;; 排空并派发处理任务执行队列中排队的消息。
;;
;; 语法
;; ----
;; (pi-app-drain-queue! app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ------
;; boolean
;; 成功出队并渲染消息时返回 #t，队列空或忙碌未处理时返回 #f。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 初始空队列返回 #f
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (check-false (pi-app-drain-queue! app))
) ;let

(check-report)
