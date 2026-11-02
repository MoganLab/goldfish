;; pi-queue-set-busy!
;; 手动将消息调度队列设置为繁忙状态（busy），常用于测试或强制锁定。
;;
;; 语法
;; ----
;; (pi-queue-set-busy! q)
;;
;; 参数
;; ----
;; q : pi-message-queue?，待处理的消息调度队列。
;;
;; 返回值
;; ----
;; unspecified，将队列状态更新为 'busy。

(import (scheme base) (liii check) (liii pi agent queue))

(check-set-mode! 'report-failed)

;; 1. 验证手动设置繁忙状态
(let ((q (make-pi-message-queue)))
  (check-true (pi-queue-idle? q))
  (pi-queue-set-busy! q)
  (check-true (pi-queue-busy? q))
  (check-false (pi-queue-idle? q))
) ;let

(check-report)
