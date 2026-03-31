(import (liii check)
        (srfi srfi-117)
) ;import

(check-set-mode! 'report-failed)

;; list-queue
;; 从元素序列直接构造队列。
;;
;; 语法
;; ----
;; (list-queue element ...) -> list-queue?

(let ((queue (list-queue)))
  (check-true (list-queue? queue))
  (check-true (list-queue-empty? queue))
  (check (list-queue-list queue) => '())
) ;let

(let ((queue (list-queue 1 2 3)))
  (check (list-queue-list queue) => '(1 2 3))
  (check (list-queue-front queue) => 1)
  (check (list-queue-back queue) => 3)
) ;let

(check-report)
