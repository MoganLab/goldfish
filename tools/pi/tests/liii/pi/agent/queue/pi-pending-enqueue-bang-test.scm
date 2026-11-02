;; pi-pending-enqueue!
;; 追加单条待注入消息到队列尾部，并返回入队后的队列长度。
;;
;; 语法
;; ----
;; (pi-pending-enqueue! q msg)
;;
;; 参数
;; ----
;; q : pi-pending-queue?，待处理的消息队列。
;; msg : any?，待追加的消息。
;;
;; 返回值
;; ----
;; integer?，入队后队列中的消息总数。

(import (scheme base) (liii check) (liii pi agent queue))

(check-set-mode! 'report-failed)

;; 1. enqueue 追加到队尾并递增返回当前长度
(let ((q (make-pi-pending-queue)))
  (check (pi-pending-enqueue! q "a") => 1)
  (check (pi-pending-enqueue! q "b") => 2)
  (check (pi-pending-enqueue! q "c") => 3)
  (check (pi-pending-empty? q) => #f)
  (check (pi-pending-items q) => '("a" "b" "c"))
) ;let

(check-report)
