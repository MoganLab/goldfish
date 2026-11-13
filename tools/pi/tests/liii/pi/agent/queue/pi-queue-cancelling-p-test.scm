;; pi-queue-cancelling?
;; 判断消息调度队列当前是否处于正在中止（cancelling）状态。
;;
;; 语法
;; ----
;; (pi-queue-cancelling? q)
;;
;; 参数
;; ----
;; q : pi-message-queue?，待检测的消息调度队列。
;;
;; 返回值
;; ----
;; boolean?，若队列状态为 'cancelling 返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii pi agent queue))

(check-set-mode! 'report-failed)

;; 1. 验证 cancelling 状态与状态转换
(let ((q (make-pi-message-queue)))
  (check-false (pi-queue-cancelling? q))
  (pi-queue-set-cancelling! q)
  (check-true (pi-queue-cancelling? q))
  (check-false (pi-queue-busy? q))
  (check-false (pi-queue-idle? q))
  (pi-queue-finish-task! q)
  (check-false (pi-queue-cancelling? q))
  (check-true (pi-queue-idle? q))
) ;let

(check-report)
