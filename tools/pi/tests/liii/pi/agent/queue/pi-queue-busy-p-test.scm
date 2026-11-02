;; pi-queue-busy?
;; 判断消息调度队列当前是否处于繁忙（busy）状态。
;;
;; 语法
;; ----
;; (pi-queue-busy? q)
;;
;; 参数
;; ----
;; q : pi-message-queue?，待检测的消息调度队列。
;;
;; 返回值
;; ----
;; boolean?，若队列状态为 'busy 返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii pi agent queue))

(check-set-mode! 'report-failed)

;; 1. 验证繁忙状态与状态转换
(let ((q (make-pi-message-queue)))
  (check-false (pi-queue-busy? q))
  (pi-queue-enqueue! q "task1")
  (pi-queue-dequeue-next! q)
  ;; 消费消息后转入 busy 状态
  (check-true (pi-queue-busy? q))
  (pi-queue-finish-task! q)
  ;; 任务完成后恢复为非 busy
  (check-false (pi-queue-busy? q))
) ;let

(check-report)
