;; make-pi-message-queue
;; 构造一个新的消息等待调度队列，初始状态为空闲（idle），队列内容为空。
;;
;; 语法
;; ----
;; (make-pi-message-queue)
;;
;; 参数
;; ----
;; 无。
;;
;; 返回值
;; ----
;; pi-message-queue?，新构造的空闲消息调度队列对象。

(import (scheme base) (liii check) (liii pi agent queue))

(check-set-mode! 'report-failed)

;; 1. 验证新创建的队列是 pi-message-queue 并且为空闲状态
(let ((q (make-pi-message-queue)))
  (check-true (pi-message-queue? q))
  (check-true (pi-queue-idle? q))
  (check-false (pi-queue-busy? q))
  (check-true (pi-queue-empty? q))
  (check (pi-queue-length q) => 0)
  (check (pi-queue-items q) => '())
) ;let

(check-report)
