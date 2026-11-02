;; pi-queue-restore-all!
;; 排空并返回消息调度队列中所有尚未被消费的消息列表。
;;
;; 语法
;; ----
;; (pi-queue-restore-all! q)
;;
;; 参数
;; ----
;; q : pi-message-queue?，待处理的消息调度队列。
;;
;; 返回值
;; ----
;; list?，包含队列中所有被排空的消息列表；若为空队列则返回 '()。

(import (scheme base) (liii check) (liii pi agent queue))

(check-set-mode! 'report-failed)

;; 1. 排空所有排队消息并清空队列
(let ((q (make-pi-message-queue)))
  (pi-queue-enqueue! q "task1")
  (pi-queue-enqueue! q "task2")
  (check (pi-queue-restore-all! q) => '("task1" "task2"))
  (check-true (pi-queue-empty? q))
  (check (pi-queue-length q) => 0)
  (check (pi-queue-restore-all! q) => '())
) ;let

(check-report)
