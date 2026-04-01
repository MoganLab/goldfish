(import (liii check)
        (liii queue))

(check-set-mode! 'report-failed)

;; list-queue-append! 基本测试
(let ((x (list-queue 1 2 3))
      (y (list-queue-copy (list-queue 4 5))))
  (let ((z (list-queue-append! x y)))
    (check (list-queue-list z) => '(1 2 3 4 5))
    ;; 第一个队列被修改
    (check (list-queue-list x) => '(1 2 3 4 5))))

;; 空参数
(let ((q (list-queue-append!)))
  (check (list-queue-empty? q) => #t))

;; 单个队列
(let ((q (list-queue 1 2 3)))
  (let ((z (list-queue-append! q)))
    (check (eq? z q) => #t)))

;; 多个队列
(let ((q1 (list-queue 1))
      (q2 (list-queue 2))
      (q3 (list-queue 3)))
  (list-queue-append! q1 q2 q3)
  (check (list-queue-list q1) => '(1 2 3)))

(check-report)
