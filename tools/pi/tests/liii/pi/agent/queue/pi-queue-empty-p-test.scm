;; pi-queue-empty?
;; 判断消息调度队列当前是否为空。
;;
;; 语法
;; ----
;; (pi-queue-empty? q)
;;
;; 参数
;; ----
;; q : pi-message-queue?，待检测的消息调度队列。
;;
;; 返回值
;; ----
;; boolean?，若队列无排队消息返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii pi agent queue))

(check-set-mode! 'report-failed)

;; 1. 新建队列为空，入队后非空，出队后恢复为空
(let ((q (make-pi-message-queue)))
  (check-true (pi-queue-empty? q))
  (pi-queue-enqueue! q "m1")
  (check-false (pi-queue-empty? q))
  (pi-queue-dequeue-next! q)
  (check-true (pi-queue-empty? q))
) ;let

(check-report)
