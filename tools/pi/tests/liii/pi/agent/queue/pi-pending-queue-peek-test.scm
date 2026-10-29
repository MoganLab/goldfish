(import (scheme base)
  (liii check)
  (liii pi agent queue)
) ;import

(check-set-mode! 'report-failed)

;; -----------------------------------------------------------------------------
;; 测试项 1: one-at-a-time 模式 peek 只看队首一条，不移除
;; -----------------------------------------------------------------------------
(let ((q (make-pi-pending-queue)))
  (pi-pending-enqueue! q "a")
  (pi-pending-enqueue! q "b")
  (check (pi-pending-peek q) => '("a"))
  ;; peek 不消费：连续两次结果一致，队列内容不变
  (check (pi-pending-peek q) => '("a"))
  (check (pi-pending-items q) => '("a" "b"))
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 2: all 模式 peek 返回全部待处理消息，不移除
;; -----------------------------------------------------------------------------
(let ((q (make-pi-pending-queue)))
  (pi-pending-enqueue! q "a")
  (pi-pending-enqueue! q "b")
  (pi-pending-mode-set! q 'all)
  (check (pi-pending-peek q) => '("a" "b"))
  (check (pi-pending-items q) => '("a" "b"))
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 3: 空队 peek 返回空表（两种模式一致）
;; -----------------------------------------------------------------------------
(let ((q (make-pi-pending-queue)))
  (check (pi-pending-peek q) => '())
  (pi-pending-mode-set! q 'all)
  (check (pi-pending-peek q) => '())
) ;let*

(check-report)
