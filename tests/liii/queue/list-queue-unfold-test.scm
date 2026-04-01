(import (liii check)
        (liii queue))

(check-set-mode! 'report-failed)

;; list-queue-unfold 基本测试
(let ((q (list-queue-unfold
           (lambda (x) (> x 3))
           (lambda (x) (* x 2))
           (lambda (x) (+ x 1))
           0)))
  ;; unfold: 递归结束后从头部添加，所以第一个元素最后被添加，保持正向顺序
  (check (list-queue-list q) => '(0 2 4 6)))

;; 带初始队列的 unfold
(let ((q0 (list-queue 8)))
  (let ((q (list-queue-unfold
             (lambda (x) (> x 3))
             (lambda (x) (* x 2))
             (lambda (x) (+ x 1))
             0
             q0)))
    (check (list-queue-list q) => '(0 2 4 6 8))))

;; 立即终止的情况
(let ((q (list-queue-unfold
           (lambda (x) (> x 0))
           (lambda (x) x)
           (lambda (x) (+ x 1))
           1)))
  (check (list-queue-empty? q) => #t))

(check-report)
