;; pi-queue-dequeue-next!
;; 从消息调度队列中消费取出队首消息并转入繁忙状态（busy）。
;;
;; 语法
;; ----
;; (pi-queue-dequeue-next! q)
;;
;; 参数
;; ----
;; q : pi-message-queue?，待处理的消息调度队列。
;;
;; 返回值
;; ----
;; any?，若队列空闲且非空则移出并返回队首消息；若已处于繁忙态或队列为空则返回 #f。

(import (scheme base) (liii check) (liii pi agent queue))

(check-set-mode! 'report-failed)

;; 1. 空闲且非空：成功消费队首消息并转入 busy
(let ((q (make-pi-message-queue)))
  (pi-queue-enqueue! q "msg1")
  (pi-queue-enqueue! q "msg2")
  (check (pi-queue-dequeue-next! q) => "msg1")
  (check-true (pi-queue-busy? q))
  ;; 处于 busy 状态时无法继续消费
  (check (pi-queue-dequeue-next! q) => #f)
  ;; 完成任务恢复 idle 后可继续消费下一条
  (pi-queue-finish-task! q)
  (check (pi-queue-dequeue-next! q) => "msg2")
) ;let

;; 2. 队列为空时返回 #f
(let ((q (make-pi-message-queue)))
  (check (pi-queue-dequeue-next! q) => #f)
) ;let

;; 3. 处于 cancelling 状态时无法消费
(let ((q (make-pi-message-queue)))
  (pi-queue-enqueue! q "msg")
  (pi-queue-set-cancelling! q)
  (check (pi-queue-dequeue-next! q) => #f)
  (pi-queue-finish-task! q)
  (check (pi-queue-dequeue-next! q) => "msg")
) ;let

(check-report)
