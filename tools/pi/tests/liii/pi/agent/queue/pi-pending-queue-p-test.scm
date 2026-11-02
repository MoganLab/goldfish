;; pi-pending-queue?
;; 判断给定对象是否为待注入消息队列 record 实例。
;;
;; 语法
;; ----
;; (pi-pending-queue? obj)
;;
;; 参数
;; ----
;; obj : any?，待检测的对象。
;;
;; 返回值
;; ----
;; boolean?，若 obj 为待注入消息队列返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii pi agent queue))

(check-set-mode! 'report-failed)

;; 1. 验证类型判断谓词
(let ((q (make-pi-pending-queue)))
  (check-true (pi-pending-queue? q))
  (check-false (pi-pending-queue? '()))
  (check-false (pi-pending-queue? #f))
  (check-false (pi-pending-queue? (make-pi-message-queue)))
) ;let

(check-report)
