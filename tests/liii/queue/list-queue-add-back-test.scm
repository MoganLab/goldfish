(import (liii check) (liii queue))


(check-set-mode! 'report-failed)


;; list-queue-add-back! 基本测试
(let ((q (list-queue 1 2)))
  (list-queue-add-back! q 3)
  (check (list-queue-list q) => '(1 2 3))
  (check (list-queue-back q) => 3)
) ;let


;; 添加到空队列
(let ((q (list-queue)))
  (list-queue-add-back! q 'a)
  (check (list-queue-list q) => '(a))
  (check (list-queue-front q) => 'a)
  (check (list-queue-back q) => 'a)
) ;let


;; 多次添加
(let ((q (list-queue)))
  (list-queue-add-back! q 1)
  (list-queue-add-back! q 2)
  (list-queue-add-back! q 3)
  (check (list-queue-list q) => '(1 2 3))
) ;let


;; 不影响前端
(let ((q (list-queue 1 2)))
  (list-queue-add-back! q 3)
  (check (list-queue-front q) => 1)
) ;let


(check-report)
