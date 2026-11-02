;; pi-pending-empty?
;; 判断待注入消息队列当前是否为空。
;;
;; 语法
;; ----
;; (pi-pending-empty? q)
;;
;; 参数
;; ----
;; q : pi-pending-queue?，待检测的待注入消息队列。
;;
;; 返回值
;; ----
;; boolean?，若队列无排队消息返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii pi agent queue))

(check-set-mode! 'report-failed)

;; 1. 验证空队列判断与状态流转
(let ((q (make-pi-pending-queue)))
  (check-true (pi-pending-empty? q))
  (pi-pending-enqueue! q "msg1")
  (check-false (pi-pending-empty? q))
  (pi-pending-drain! q)
  (check-true (pi-pending-empty? q))
) ;let

(check-report)
