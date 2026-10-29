(import (scheme base)
  (liii check)
  (liii pi agent queue)
) ;import

(check-set-mode! 'report-failed)

;; -----------------------------------------------------------------------------
;; 测试项 1: 默认构造为空队列，模式为 one-at-a-time
;; -----------------------------------------------------------------------------
(let ((q (make-pi-pending-queue)))
  (check (pi-pending-queue? q) => #t)
  (check (pi-pending-mode q) => 'one-at-a-time)
  (check (pi-pending-empty? q) => #t)
  (check (pi-pending-length q) => 0)
  (check (pi-pending-items q) => '())
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 2: enqueue 追加到队尾并推进长度
;; -----------------------------------------------------------------------------
(let ((q (make-pi-pending-queue)))
  (check (pi-pending-enqueue! q "a") => 1)
  (check (pi-pending-enqueue! q "b") => 2)
  (check (pi-pending-enqueue! q "c") => 3)
  (check (pi-pending-empty? q) => #f)
  (check (pi-pending-items q) => '("a" "b" "c"))
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 3: mode-set! 切换模式；非法模式抛 type-error
;; -----------------------------------------------------------------------------
(let ((q (make-pi-pending-queue)))
  (pi-pending-mode-set! q 'all)
  (check (pi-pending-mode q) => 'all)
  (pi-pending-mode-set! q 'one-at-a-time)
  (check (pi-pending-mode q) => 'one-at-a-time)
  (check-catch 'type-error (pi-pending-mode-set! q 'bogus))
) ;let*

(check-report)
