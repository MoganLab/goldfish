(import (liii check)
        (srfi srfi-117)
) ;import

(check-set-mode! 'report-failed)

;; list-queue-concatenate
;; 将队列列表拼接为一个新队列。
;;
;; 语法
;; ----
;; (list-queue-concatenate queues) -> list-queue?

(let* ((queue-1 (list-queue 1))
       (queue-2 (list-queue))
       (queue-3 (list-queue 2 3))
       (queue (list-queue-concatenate (list queue-1 queue-2 queue-3)))
) ;let*
  (check (list-queue-list queue) => '(1 2 3))
  (check (list-queue-list queue-1) => '(1))
  (check (list-queue-list queue-2) => '())
  (check (list-queue-list queue-3) => '(2 3))
) ;let*

;; 错误处理测试
(check-catch 'wrong-type-arg
             (list-queue-concatenate (list (list-queue 1) 2)))

(check-report)
