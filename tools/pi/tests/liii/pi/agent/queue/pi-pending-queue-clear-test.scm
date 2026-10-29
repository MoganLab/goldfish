(import (scheme base)
  (liii check)
  (liii pi agent queue)
) ;import

(check-set-mode! 'report-failed)

;; -----------------------------------------------------------------------------
;; 测试项 1: clear 清空队列并返回被移除的全部消息（ESC 撤回用）
;; -----------------------------------------------------------------------------
(let ((q (make-pi-pending-queue)))
  (pi-pending-enqueue! q "a")
  (pi-pending-enqueue! q "b")
  (pi-pending-enqueue! q "c")
  (check (pi-pending-clear! q) => '("a" "b" "c"))
  (check (pi-pending-empty? q) => #t)
  (check (pi-pending-length q) => 0)
  (check (pi-pending-clear! q) => '())
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 2: clear 后可继续入队使用
;; -----------------------------------------------------------------------------
(let ((q (make-pi-pending-queue)))
  (pi-pending-enqueue! q "a")
  (pi-pending-clear! q)
  (pi-pending-enqueue! q "b")
  (check (pi-pending-drain! q) => '("b"))
) ;let*

(check-report)
