(import (liii check)
        (liii queue))

(check-set-mode! 'report-failed)

;; list-queue-copy 基本测试
(let* ((q1 (list-queue 1 2 3))
       (q2 (list-queue-copy q1)))
  (check (list-queue-list q2) => '(1 2 3))
  ;; 修改 q2 不影响 q1
  (list-queue-add-front! q2 0)
  (check (list-queue-list q1) => '(1 2 3))
  (check (list-queue-list q2) => '(0 1 2 3)))

;; 复制空队列
(let ((q (list-queue-copy (list-queue))))
  (check (list-queue-empty? q) => #t))

;; 复制后添加元素到原队列不影响副本
(let* ((q1 (list-queue 'a 'b))
       (q2 (list-queue-copy q1)))
  (list-queue-add-back! q1 'c)
  (check (list-queue-list q1) => '(a b c))
  (check (list-queue-list q2) => '(a b)))

(check-report)
