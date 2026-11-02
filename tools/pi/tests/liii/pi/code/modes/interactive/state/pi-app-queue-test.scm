;; pi-app-queue
;; 获取应用实例的任务执行队列。
;;
;; 语法
;; ----
;; (pi-app-queue app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ------
;; any
;; 应用的任务队列对象。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi agent queue)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 验证获取有效的任务队列
(let* ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24)))
       (q (pi-app-queue app))
      ) ;
  (check-true (pi-message-queue? q))
  (check-true (pi-queue-empty? q))
) ;let*

(check-report)
