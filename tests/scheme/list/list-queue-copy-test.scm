(import (liii check)
        (srfi srfi-117)
) ;import

(check-set-mode! 'report-failed)

;; list-queue-copy
;; 复制队列，返回一个内容相同但可独立修改的新队列。
;;
;; 语法
;; ----
;; (list-queue-copy queue) -> list-queue?

(let* ((queue (list-queue 1 2 3))
       (copy (list-queue-copy queue))
) ;let*
  (list-queue-add-front! copy 0)
  (check (list-queue-list queue) => '(1 2 3))
  (check (list-queue-list copy) => '(0 1 2 3))
) ;let*

(check-catch 'wrong-type-arg (list-queue-copy 1))

(check-report)
