;; pi-app-jobs-chan
;; 获取当前应用实例的任务分发通道。
;;
;; 语法
;; ----
;; (pi-app-jobs-chan app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ------
;; any
;; 任务分发通道对象。

(import (scheme base)
  (liii check)
  (liii go)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 验证获取非空通道
(let* ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24)))
       (ch (pi-app-jobs-chan app))
      ) ;
  (check-true (chan? ch))
) ;let*

(check-report)
