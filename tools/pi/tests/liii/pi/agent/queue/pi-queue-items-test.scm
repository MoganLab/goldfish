;; pi-queue-items
;; 获取消息调度队列中所有等待被消费的消息列表。
;;
;; 语法
;; ----
;; (pi-queue-items q)
;;
;; 参数
;; ----
;; q : pi-message-queue?，待检测的消息调度队列。
;;
;; 返回值
;; ----
;; list?，排队消息列表。

(import (scheme base) (liii check) (liii pi agent queue))

(check-set-mode! 'report-failed)

;; 1. 验证获取队列元素列表
(let ((q (make-pi-message-queue)))
  (check (pi-queue-items q) => '())
  (pi-queue-enqueue! q "first")
  (pi-queue-enqueue! q "second")
  (check (pi-queue-items q) => '("first" "second"))
) ;let

(check-report)
