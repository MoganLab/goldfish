(import (liii check)
        (liii queue))

(check-set-mode! 'report-failed)

;; list-queue-add-front! 基本测试
(let ((q (list-queue 2 3)))
  (list-queue-add-front! q 1)
  (check (list-queue-list q) => '(1 2 3))
  (check (list-queue-front q) => 1))

;; 添加到空队列
(let ((q (list-queue)))
  (list-queue-add-front! q 'a)
  (check (list-queue-list q) => '(a))
  (check (list-queue-front q) => 'a)
  (check (list-queue-back q) => 'a))

;; 多次添加
(let ((q (list-queue)))
  (list-queue-add-front! q 3)
  (list-queue-add-front! q 2)
  (list-queue-add-front! q 1)
  (check (list-queue-list q) => '(1 2 3)))

;; 不影响后端
(let ((q (list-queue 2 3)))
  (list-queue-add-front! q 1)
  (check (list-queue-back q) => 3))

(check-report)
