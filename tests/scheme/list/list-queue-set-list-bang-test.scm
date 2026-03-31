(import (liii check)
        (srfi srfi-117)
) ;import

(check-set-mode! 'report-failed)

;; list-queue-set-list!
;; 直接替换队列内部列表。
;;
;; 语法
;; ----
;; (list-queue-set-list! queue first)
;; (list-queue-set-list! queue first last)

(let* ((pairs (list 5 6 7))
       (last (cddr pairs))
       (queue (list-queue 1 2))
) ;let*
  (list-queue-set-list! queue pairs last)
  (check (list-queue-list queue) => '(5 6 7))
  (list-queue-add-back! queue 8)
  (check pairs => '(5 6 7 8))
) ;let*

(let ((queue (list-queue 9 10)))
  (list-queue-set-list! queue '(4 5))
  (check (list-queue-list queue) => '(4 5))
) ;let

(let ((queue (list-queue 9 10)))
  (list-queue-set-list! queue '())
  (check-true (list-queue-empty? queue))
  (check (list-queue-list queue) => '())
) ;let

;; 错误处理测试
(check-catch 'wrong-type-arg (list-queue-set-list! (list-queue) 1))

(check-report)
