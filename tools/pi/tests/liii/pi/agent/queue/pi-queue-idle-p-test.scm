;; pi-queue-idle?
;; 判断消息调度队列当前是否处于空闲（idle）状态。
;;
;; 语法
;; ----
;; (pi-queue-idle? q)
;;
;; 参数
;; ----
;; q : pi-message-queue?，待检测的消息调度队列。
;;
;; 返回值
;; ----
;; boolean?，若队列状态为 'idle 返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii pi agent queue))

(check-set-mode! 'report-failed)

;; 1. 验证空闲状态与状态转换
(let ((q (make-pi-message-queue)))
  (check-true (pi-queue-idle? q))
  (pi-queue-enqueue! q "task1")
  ;; 入队后仍为 idle，直到 dequeue 消费
  (check-true (pi-queue-idle? q))
  (pi-queue-dequeue-next! q)
  (check-false (pi-queue-idle? q))
  (pi-queue-finish-task! q)
  (check-true (pi-queue-idle? q))
) ;let

(check-report)
