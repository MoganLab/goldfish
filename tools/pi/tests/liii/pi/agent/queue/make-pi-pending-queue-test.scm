;; make-pi-pending-queue
;; 构造一个新的待注入消息队列，默认采用单条投递模式（'one-at-a-time），初始为空。
;;
;; 语法
;; ----
;; (make-pi-pending-queue)
;;
;; 参数
;; ----
;; 无。
;;
;; 返回值
;; ----
;; pi-pending-queue?，新构造的待注入消息队列对象。

(import (scheme base) (liii check) (liii pi agent queue))

(check-set-mode! 'report-failed)

;; 1. 验证新创建的待注入消息队列
(let ((q (make-pi-pending-queue)))
  (check-true (pi-pending-queue? q))
  (check (pi-pending-mode q) => 'one-at-a-time)
  (check-true (pi-pending-empty? q))
  (check (pi-pending-length q) => 0)
  (check (pi-pending-items q) => '())
) ;let

(check-report)
