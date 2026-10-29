(import (scheme base)
  (liii check)
  (liii pi agent queue)
) ;import

(check-set-mode! 'report-failed)

;; -----------------------------------------------------------------------------
;; 测试项 1: one-at-a-time 模式 drain 每次只取走队首一条
;; -----------------------------------------------------------------------------
(let ((q (make-pi-pending-queue)))
  (pi-pending-enqueue! q "a")
  (pi-pending-enqueue! q "b")
  (check (pi-pending-drain! q) => '("a"))
  (check (pi-pending-items q) => '("b"))
  (check (pi-pending-drain! q) => '("b"))
  (check (pi-pending-empty? q) => #t)
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 2: all 模式 drain 一次性取走全部
;; -----------------------------------------------------------------------------
(let ((q (make-pi-pending-queue)))
  (pi-pending-enqueue! q "a")
  (pi-pending-enqueue! q "b")
  (pi-pending-mode-set! q 'all)
  (check (pi-pending-drain! q) => '("a" "b"))
  (check (pi-pending-empty? q) => #t)
  (check (pi-pending-drain! q) => '())
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 3: drain 后再 enqueue 的新消息正常排队（FIFO 语义保持）
;; -----------------------------------------------------------------------------
(let ((q (make-pi-pending-queue)))
  (pi-pending-enqueue! q "a")
  (check (pi-pending-drain! q) => '("a"))
  (pi-pending-enqueue! q "b")
  (check (pi-pending-drain! q) => '("b"))
) ;let*

(check-report)
