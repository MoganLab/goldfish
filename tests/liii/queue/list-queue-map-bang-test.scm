(import (liii check)
        (liii queue))

(check-set-mode! 'report-failed)

;; list-queue-map! 基本测试
(let ((q (list-queue 1 2 3)))
  (list-queue-map! (lambda (x) (* x 10)) q)
  (check (list-queue-list q) => '(10 20 30)))

;; 空队列
(let ((q (list-queue)))
  (list-queue-map! (lambda (x) (* x 2)) q)
  (check (list-queue-empty? q) => #t))

;; 单元素队列
(let ((q (list-queue 42)))
  (list-queue-map! (lambda (x) (+ x 1)) q)
  (check (list-queue-list q) => '(43)))

;; 修改后可以继续操作
(let ((q (list-queue 1 2 3)))
  (list-queue-map! (lambda (x) (* x 2)) q)
  (list-queue-add-back! q 100)
  (check (list-queue-list q) => '(2 4 6 100)))

(check-report)
