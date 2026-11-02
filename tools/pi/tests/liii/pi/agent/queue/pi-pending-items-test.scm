;; pi-pending-items
;; 获取待注入消息队列中当前所有排队消息的列表。
;;
;; 语法
;; ----
;; (pi-pending-items q)
;;
;; 参数
;; ----
;; q : pi-pending-queue?，待检测的待注入消息队列。
;;
;; 返回值
;; ----
;; list?，排队消息列表。

(import (scheme base) (liii check) (liii pi agent queue))

(check-set-mode! 'report-failed)

;; 1. 验证获取队列元素列表
(let ((q (make-pi-pending-queue)))
  (check (pi-pending-items q) => '())
  (pi-pending-enqueue! q "first")
  (pi-pending-enqueue! q "second")
  (check (pi-pending-items q) => '("first" "second"))
) ;let

(check-report)
