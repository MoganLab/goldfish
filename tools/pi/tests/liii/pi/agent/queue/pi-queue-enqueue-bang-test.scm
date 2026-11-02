;; pi-queue-enqueue!
;; 追加消息到消息调度队列尾部，并返回当前排队消息总数。
;;
;; 语法
;; ----
;; (pi-queue-enqueue! q msg)
;;
;; 参数
;; ----
;; q : pi-message-queue?，待处理的消息调度队列。
;; msg : any?，待追加的消息。
;;
;; 返回值
;; ----
;; integer?，入队后队列中的排队消息总数。

(import (scheme base) (liii check) (liii pi agent queue))

(check-set-mode! 'report-failed)

;; 1. 追加消息到队尾并返回新长度
(let ((q (make-pi-message-queue)))
  (check (pi-queue-enqueue! q "taskA") => 1)
  (check (pi-queue-enqueue! q "taskB") => 2)
  (check (pi-queue-items q) => '("taskA" "taskB"))
) ;let

(check-report)
