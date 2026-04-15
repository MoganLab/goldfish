(import (liii check) (liii queue))


(check-set-mode! 'report-failed)


;; make-list-queue 基本测试
(let ((q (make-list-queue '(1 2 3))))
  (check (list-queue-list q) => '(1 2 3))
) ;let


;; 空列表
(let ((q (make-list-queue '())))
  (check (list-queue-empty? q) => #t)
) ;let


;; 带 last-pair 参数的 make-list-queue
(let* ((lst '(1 2 3))
       (last-ptr (cddr lst))
       (q (make-list-queue lst last-ptr))
      ) ;
  (check (list-queue-list q) => '(1 2 3))
  (check (list-queue-back q) => 3)
) ;let*


(check-report)
