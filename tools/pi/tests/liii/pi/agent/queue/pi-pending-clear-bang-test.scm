;; pi-pending-clear!
;; 清空待注入消息队列中的全部消息并返回被移除的消息列表。
;;
;; 语法
;; ----
;; (pi-pending-clear! q)
;;
;; 参数
;; ----
;; q : pi-pending-queue?，待处理的消息队列。
;;
;; 返回值
;; ----
;; list?，包含队列中原本所有消息的列表；若原队列为空则返回 '()。

(import (scheme base) (liii check) (liii pi agent queue))

(check-set-mode! 'report-failed)

;; 1. clear 清空队列并返回被移除的全部消息（ESC 撤回用）
(let ((q (make-pi-pending-queue)))
  (pi-pending-enqueue! q "a")
  (pi-pending-enqueue! q "b")
  (pi-pending-enqueue! q "c")
  (check (pi-pending-clear! q) => '("a" "b" "c"))
  (check (pi-pending-empty? q) => #t)
  (check (pi-pending-length q) => 0)
  (check (pi-pending-clear! q) => '())
) ;let

;; 2. clear 后可继续入队使用
(let ((q (make-pi-pending-queue)))
  (pi-pending-enqueue! q "a")
  (pi-pending-clear! q)
  (pi-pending-enqueue! q "b")
  (check (pi-pending-drain! q) => '("b"))
) ;let

(check-report)
