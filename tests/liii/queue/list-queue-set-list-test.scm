(import (liii check) (liii queue))


(check-set-mode! 'report-failed)


;; list-queue-set-list! 基本测试（单参数）
(let ((q (list-queue 1 2 3)))
  (list-queue-set-list! q '(a b c))
  (check (list-queue-list q) => '(a b c))
  (check (list-queue-front q) => 'a)
  (check (list-queue-back q) => 'c)
) ;let


;; 设置为空列表
(let ((q (list-queue 1 2 3)))
  (list-queue-set-list! q '())
  (check (list-queue-empty? q) => #t)
) ;let


;; 双参数形式
(let* ((lst '(1 2 3))
       (last-ptr (cddr lst))
       (q (list-queue))
      ) ;
  (list-queue-set-list! q lst last-ptr)
  (check (list-queue-list q) => '(1 2 3))
) ;let*


;; 设置后队列正常工作
(let ((q (list-queue)))
  (list-queue-set-list! q '(1 2))
  (list-queue-add-back! q 3)
  (list-queue-add-front! q 0)
  (check (list-queue-list q)
    =>
    '(0 1 2 3)
  ) ;check
) ;let


(check-report)
