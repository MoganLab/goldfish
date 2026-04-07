(import (liii check)
        (liii queue)
) ;import

(check-set-mode! 'report-failed)

;; list-queue-unfold-right 基本测试
(let ((q (list-queue-unfold-right
           (lambda (x) (> x 3))
           (lambda (x) (* x 2))
           (lambda (x) (+ x 1))
           0)))
  ;; unfold-right: 递归结束后从尾部添加，所以第一个元素最后被添加，结果反向
  (check (list-queue-list q) => '(6 4 2 0))
) ;let

;; 带初始队列的 unfold-right
(let ((q0 (list-queue 8)))
  (let ((q (list-queue-unfold-right
             (lambda (x) (> x 3))
             (lambda (x) (* x 2))
             (lambda (x) (+ x 1))
             0
             q0)))
    (check (list-queue-list q) => '(8 6 4 2 0))
  ) ;let
) ;let

;; 立即终止的情况
(let ((q (list-queue-unfold-right
           (lambda (x) (> x 0))
           (lambda (x) x)
           (lambda (x) (+ x 1))
           1)))
  (check (list-queue-empty? q) => #t)
) ;let

(check-report)
