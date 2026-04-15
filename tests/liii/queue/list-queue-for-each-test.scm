(import (liii check) (liii queue))


(check-set-mode! 'report-failed)


;; list-queue-for-each 基本测试
(let ((q (list-queue 1 2 3)) (sum 0))
  (list-queue-for-each (lambda (x) (set! sum (+ sum x)))
    q
  ) ;list-queue-for-each
  (check sum => 6)
) ;let


;; 空队列
(let ((q (list-queue)) (count 0))
  (list-queue-for-each (lambda (x) (set! count (+ count 1)))
    q
  ) ;list-queue-for-each
  (check count => 0)
) ;let


;; 单元素队列
(let ((q (list-queue 42)) (val 0))
  (list-queue-for-each (lambda (x) (set! val x))
    q
  ) ;list-queue-for-each
  (check val => 42)
) ;let


;; 收集元素
(let ((q (list-queue 'a 'b 'c)) (result '()))
  (list-queue-for-each (lambda (x)
                         (set! result (cons x result))
                       ) ;lambda
    q
  ) ;list-queue-for-each
  (check result => '(c b a))
) ;let


(check-report)
