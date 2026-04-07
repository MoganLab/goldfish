(import (liii check)
        (liii queue)
) ;import

(check-set-mode! 'report-failed)

;; list-queue-empty? 基本测试
(check (list-queue-empty? (list-queue)) => #t)
(check (list-queue-empty? (list-queue 1)) => #f)
(check (list-queue-empty? (list-queue 1 2 3)) => #f)

;; 移除所有元素后为空
(let ((q (list-queue 1 2 3)))
  (list-queue-remove-all! q)
  (check (list-queue-empty? q) => #t)
) ;let

;; 从前端移除所有元素
(let ((q (list-queue 1 2)))
  (list-queue-remove-front! q)
  (list-queue-remove-front! q)
  (check (list-queue-empty? q) => #t)
) ;let

;; 从后端移除所有元素
(let ((q (list-queue 1 2)))
  (list-queue-remove-back! q)
  (list-queue-remove-back! q)
  (check (list-queue-empty? q) => #t)
) ;let

(check-report)
