(import (liii check)
        (srfi srfi-117)
) ;import

(check-set-mode! 'report-failed)

;; list-queue-remove-all!
;; 删除全部元素并返回原列表。
;;
;; 语法
;; ----
;; (list-queue-remove-all! queue) -> list?

(let ((queue (list-queue 1 2 3)))
  (check (list-queue-remove-all! queue) => '(1 2 3))
  (check-true (list-queue-empty? queue))
) ;let

(let ((queue (list-queue)))
  (check (list-queue-remove-all! queue) => '())
  (check-true (list-queue-empty? queue))
) ;let

(check-catch 'wrong-type-arg (list-queue-remove-all! 1))

(check-report)
