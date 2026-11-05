;; pi-app-restore-queues!
;; 清空并回收待处理的 steering、follow-up 和主队列消息，返回回收的所有消息文本列表。
;;
;; 语法
;; ----
;; (pi-app-restore-queues! app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ----
;; list?
;; 回收到的全部未消费消息文本列表。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
  (liii pi code modes interactive app-queue)
) ;import

(check-set-mode! 'report-failed)

;; 1. 回收 pending 消息并清空状态
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm)))
  (pi-app-queue-steering! app "steer msg")
  (pi-app-queue-follow-up! app "follow msg")
  (let ((all (pi-app-restore-queues! app)))
    (check-true (and (member "steer msg" all) #t))
    (check-true (and (member "follow msg" all) #t))
    (check (%pi-app-steering-pending app) => '())
    (check (%pi-app-follow-up-pending app) => '())))

(check-report)
