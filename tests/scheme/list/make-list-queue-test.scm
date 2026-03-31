(import (liii check)
        (srfi srfi-117)
) ;import

(check-set-mode! 'report-failed)

;; make-list-queue
;; 从列表或 first/last pair 构造队列。
;;
;; 语法
;; ----
;; (make-list-queue first)
;; (make-list-queue first last)

(let ((queue (make-list-queue '())))
  (check-true (list-queue? queue))
  (check-true (list-queue-empty? queue))
  (check (list-queue-list queue) => '())
) ;let

(let ((queue (make-list-queue '(1 2 3))))
  (check (list-queue-empty? queue) => #f)
  (check (list-queue-list queue) => '(1 2 3))
  (check (list-queue-front queue) => 1)
  (check (list-queue-back queue) => 3)
) ;let

(let* ((pairs (list 'a 'b 'c))
       (queue (make-list-queue pairs (cddr pairs)))
) ;let*
  (list-queue-add-back! queue 'd)
  (check pairs => '(a b c d))
  (check (list-queue-list queue) => '(a b c d))
) ;let*

;; 错误处理测试
(check-catch 'wrong-type-arg (make-list-queue 1))
(check-catch 'wrong-type-arg (make-list-queue '() '(1)))

(check-report)
