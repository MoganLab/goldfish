(import (liii check)
        (liii queue)
) ;import

(check-set-mode! 'report-failed)

;; list-queue-concatenate 基本测试
(let ((queues (list (list-queue 1 2 3) (list-queue 4 5 6))))
  (let ((z (list-queue-concatenate queues)))
    (check (list-queue-list z) => '(1 2 3 4 5 6))
  ) ;let
) ;let

;; 空列表
(let ((z (list-queue-concatenate '())))
  (check (list-queue-empty? z) => #t)
) ;let

;; 单个队列
(let ((queues (list (list-queue 1 2 3))))
  (let ((z (list-queue-concatenate queues)))
    (check (list-queue-list z) => '(1 2 3))
  ) ;let
) ;let

;; 多个队列
(let ((queues (list (list-queue 1) (list-queue 2) (list-queue 3) (list-queue 4))))
  (let ((z (list-queue-concatenate queues)))
    (check (list-queue-list z) => '(1 2 3 4))
  ) ;let
) ;let

;; 包含空队列
(let ((queues (list (list-queue 1 2) (list-queue) (list-queue 3 4))))
  (let ((z (list-queue-concatenate queues)))
    (check (list-queue-list z) => '(1 2 3 4))
  ) ;let
) ;let

(check-report)
