(import (liii check)
        (liii queue))

(check-set-mode! 'report-failed)

;; list-queue-for-each 基本测试
(let ((q (list-queue 1 2 3))
      (sum 0))
  (list-queue-for-each (lambda (x) (set! sum (+ sum x))) q)
  (check sum => 6))

;; 空队列
(let ((q (list-queue))
      (count 0))
  (list-queue-for-each (lambda (x) (set! count (+ count 1))) q)
  (check count => 0))

;; 单元素队列
(let ((q (list-queue 42))
      (val 0))
  (list-queue-for-each (lambda (x) (set! val x)) q)
  (check val => 42))

;; 收集元素
(let ((q (list-queue 'a 'b 'c))
      (result '()))
  (list-queue-for-each (lambda (x) (set! result (cons x result))) q)
  (check result => '(c b a)))

(check-report)
