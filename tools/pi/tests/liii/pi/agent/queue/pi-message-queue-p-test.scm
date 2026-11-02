;; pi-message-queue?
;; 判断给定对象是否为消息调度队列 record 实例。
;;
;; 语法
;; ----
;; (pi-message-queue? obj)
;;
;; 参数
;; ----
;; obj : any?，待检测的对象。
;;
;; 返回值
;; ----
;; boolean?，若 obj 为消息调度队列则返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii pi agent queue))

(check-set-mode! 'report-failed)

;; 1. 验证真值与假值
(let ((q (make-pi-message-queue)))
  (check-true (pi-message-queue? q))
  (check-false (pi-message-queue? '()))
  (check-false (pi-message-queue? #f))
  (check-false (pi-message-queue? (make-pi-pending-queue)))
) ;let

(check-report)
