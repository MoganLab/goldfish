(import (liii check)
        (liii queue)
) ;import

(check-set-mode! 'report-failed)

;; list-queue 基本测试
(let ((q (list-queue 1 2 3)))
  (check (list-queue-list q) => '(1 2 3))
) ;let

;; 空队列
(let ((q (list-queue)))
  (check (list-queue-empty? q) => #t)
) ;let

;; 单个元素
(let ((q (list-queue 42)))
  (check (list-queue-front q) => 42)
  (check (list-queue-back q) => 42)
) ;let

;; 多个不同类型的元素
(let ((q (list-queue 'a "hello" 123 #t)))
  (check (list-queue-front q) => 'a)
  (check (list-queue-back q) => #t)
) ;let

(check-report)
