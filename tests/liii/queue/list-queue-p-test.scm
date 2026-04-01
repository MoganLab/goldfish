(import (liii check)
        (liii queue))

(check-set-mode! 'report-failed)

;; list-queue? 基本测试
(check (list-queue? (list-queue 1 2 3)) => #t)
(check (list-queue? (list-queue)) => #t)
(check (list-queue? (make-list-queue '(1 2 3))) => #t)

;; 非队列类型
(check (list-queue? '()) => #f)
(check (list-queue? '(1 2 3)) => #f)
(check (list-queue? 42) => #f)
(check (list-queue? "queue") => #f)
(check (list-queue? #t) => #f)
(check (list-queue? 'sym) => #f)
(check (list-queue? (list 1 2 3)) => #f)

;; 复制后的队列仍然是队列
(let ((q (list-queue-copy (list-queue 1 2 3))))
  (check (list-queue? q) => #t))

(check-report)
