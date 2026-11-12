;; pi-queue-set-cancelling!
;; 将消息调度队列设置为正在中止状态（cancelling），供 ESC 中止当前任务时使用。
;;
;; 语法
;; ----
;; (pi-queue-set-cancelling! q)
;;
;; 参数
;; ----
;; q : pi-message-queue?，待处理的消息调度队列。
;;
;; 返回值
;; ----
;; unspecified，将队列状态更新为 'cancelling。

(import (scheme base) (liii check) (liii pi agent queue))

(check-set-mode! 'report-failed)

;; 1. 验证设置 cancelling 状态
(let ((q (make-pi-message-queue)))
  (check-true (pi-queue-idle? q))
  (pi-queue-set-busy! q)
  (check-true (pi-queue-busy? q))
  (pi-queue-set-cancelling! q)
  (check-true (pi-queue-cancelling? q))
  (check-false (pi-queue-busy? q))
  (check-false (pi-queue-idle? q))
) ;let

(check-report)
