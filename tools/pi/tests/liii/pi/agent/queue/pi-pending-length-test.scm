;; pi-pending-length
;; 获取待注入消息队列中当前排队的消息数量。
;;
;; 语法
;; ----
;; (pi-pending-length q)
;;
;; 参数
;; ----
;; q : pi-pending-queue?，待检测的待注入消息队列。
;;
;; 返回值
;; ----
;; integer?，排队消息总数。

(import (scheme base) (liii check) (liii pi agent queue))

(check-set-mode! 'report-failed)

;; 1. 验证队列长度变化
(let ((q (make-pi-pending-queue)))
  (check (pi-pending-length q) => 0)
  (pi-pending-enqueue! q "m1")
  (check (pi-pending-length q) => 1)
  (pi-pending-enqueue! q "m2")
  (check (pi-pending-length q) => 2)
  (pi-pending-drain! q)
  (check (pi-pending-length q) => 1)
) ;let

(check-report)
