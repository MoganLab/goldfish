;; pi-queue-finish-task!
;; 通知消息调度队列当前任务执行完成，将队列状态恢复为空闲（idle）。
;;
;; 语法
;; ----
;; (pi-queue-finish-task! q)
;;
;; 参数
;; ----
;; q : pi-message-queue?，待处理的消息调度队列。
;;
;; 返回值
;; ----
;; unspecified，将队列状态更新为 'idle。

(import (scheme base) (liii check) (liii pi agent queue))

(check-set-mode! 'report-failed)

;; 1. 验证完成任务后解除 busy 锁定
(let ((q (make-pi-message-queue)))
  (pi-queue-enqueue! q "task1")
  (pi-queue-dequeue-next! q)
  (check-true (pi-queue-busy? q))
  (pi-queue-finish-task! q)
  (check-true (pi-queue-idle? q))
  (check-false (pi-queue-busy? q))
) ;let

(check-report)
