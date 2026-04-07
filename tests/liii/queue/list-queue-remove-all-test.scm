(import (liii check)
        (liii queue)
) ;import

(check-set-mode! 'report-failed)

;; list-queue-remove-all! 基本测试
(let ((q (list-queue 1 2 3)))
  (check (list-queue-remove-all! q) => '(1 2 3))
  (check (list-queue-empty? q) => #t)
) ;let

;; 空队列
(let ((q (list-queue)))
  (check (list-queue-remove-all! q) => '())
  (check (list-queue-empty? q) => #t)
) ;let

;; 单元素队列
(let ((q (list-queue 42)))
  (check (list-queue-remove-all! q) => '(42))
  (check (list-queue-empty? q) => #t)
) ;let

;; 移除后可以重新使用
(let ((q (list-queue 1 2)))
  (list-queue-remove-all! q)
  (list-queue-add-back! q 3)
  (list-queue-add-front! q 0)
  (check (list-queue-list q) => '(0 3))
) ;let

(check-report)
