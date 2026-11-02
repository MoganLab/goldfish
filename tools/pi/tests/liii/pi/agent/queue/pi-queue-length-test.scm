;; pi-queue-length
;; 获取消息调度队列中当前等待消费的消息数量。
;;
;; 语法
;; ----
;; (pi-queue-length q)
;;
;; 参数
;; ----
;; q : pi-message-queue?，待检测的消息调度队列。
;;
;; 返回值
;; ----
;; integer?，排队消息总数。

(import (scheme base) (liii check) (liii pi agent queue))

(check-set-mode! 'report-failed)

;; 1. 验证队列长度变化
(let ((q (make-pi-message-queue)))
  (check (pi-queue-length q) => 0)
  (pi-queue-enqueue! q "m1")
  (check (pi-queue-length q) => 1)
  (pi-queue-enqueue! q "m2")
  (check (pi-queue-length q) => 2)
  (pi-queue-dequeue-next! q)
  (check (pi-queue-length q) => 1)
) ;let

(check-report)
