(import (liii check)
        (liii queue))

(check-set-mode! 'report-failed)

;; list-queue-remove-back! 基本测试
(let ((q (list-queue 1 2 3)))
  (check (list-queue-remove-back! q) => 3)
  (check (list-queue-list q) => '(1 2)))

;; 单元素队列
(let ((q (list-queue 42)))
  (check (list-queue-remove-back! q) => 42)
  (check (list-queue-empty? q) => #t))

;; 移除所有元素
(let ((q (list-queue 1 2 3)))
  (check (list-queue-remove-back! q) => 3)
  (check (list-queue-remove-back! q) => 2)
  (check (list-queue-remove-back! q) => 1)
  (check (list-queue-empty? q) => #t))

;; 移除后可以继续添加
(let ((q (list-queue 1 2)))
  (list-queue-remove-back! q)
  (list-queue-add-back! q 3)
  (check (list-queue-list q) => '(1 3)))

;; 空队列报错
(check-catch 'wrong-type-arg (list-queue-remove-back! (list-queue)))

(check-report)
