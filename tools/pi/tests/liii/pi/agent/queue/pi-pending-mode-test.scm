;; pi-pending-mode
;; 获取待注入消息队列当前的投递模式（'one-at-a-time 或 'all）。
;;
;; 语法
;; ----
;; (pi-pending-mode q)
;;
;; 参数
;; ----
;; q : pi-pending-queue?，待查询的待注入消息队列。
;;
;; 返回值
;; ----
;; symbol?，当前的投递模式符号（'one-at-a-time 或 'all）。

(import (scheme base) (liii check) (liii pi agent queue))

(check-set-mode! 'report-failed)

;; 1. 默认模式为 'one-at-a-time
(let ((q (make-pi-pending-queue)))
  (check (pi-pending-mode q) => 'one-at-a-time)
  (pi-pending-mode-set! q 'all)
  (check (pi-pending-mode q) => 'all)
) ;let

(check-report)
