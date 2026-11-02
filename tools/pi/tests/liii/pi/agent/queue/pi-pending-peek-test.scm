;; pi-pending-peek
;; 预览下次投递即将取出的消息列表，但不从队列中移除。
;;
;; 语法
;; ----
;; (pi-pending-peek q)
;;
;; 参数
;; ----
;; q : pi-pending-queue?，待查看的消息队列。
;;
;; 返回值
;; ----
;; list?，根据队列模式返回队首单条消息列表或全部消息列表；空队列返回 '()。

(import (scheme base) (liii check) (liii pi agent queue))

(check-set-mode! 'report-failed)

;; 1. one-at-a-time 模式 peek 只看队首一条，不移除
(let ((q (make-pi-pending-queue)))
  (pi-pending-enqueue! q "a")
  (pi-pending-enqueue! q "b")
  (check (pi-pending-peek q) => '("a"))
  ;; peek 不消费：连续两次结果一致，队列内容不变
  (check (pi-pending-peek q) => '("a"))
  (check (pi-pending-items q) => '("a" "b"))
) ;let

;; 2. all 模式 peek 返回全部待处理消息，不移除
(let ((q (make-pi-pending-queue)))
  (pi-pending-enqueue! q "a")
  (pi-pending-enqueue! q "b")
  (pi-pending-mode-set! q 'all)
  (check (pi-pending-peek q) => '("a" "b"))
  (check (pi-pending-items q) => '("a" "b"))
) ;let

;; 3. 空队 peek 返回空表（两种模式一致）
(let ((q (make-pi-pending-queue)))
  (check (pi-pending-peek q) => '())
  (pi-pending-mode-set! q 'all)
  (check (pi-pending-peek q) => '())
) ;let

(check-report)
