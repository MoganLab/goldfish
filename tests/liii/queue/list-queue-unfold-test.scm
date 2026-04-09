(import (liii check)
        (liii queue)
) ;import

(check-set-mode! 'report-failed)

;; list-queue-unfold 基本测试
(let
  ((q
     (list-queue-unfold
       (lambda (x) (> x 3))
       (lambda (x) (* x 2))
       (lambda (x) (+ x 1))
       0)
     ) ;list-queue-unfold
   ) ;q
  ;; unfold: 递归结束后从头部添加，所以第一个元素最后被添加，保持正向顺序
  (check (list-queue-list q) => '(0 2 4 6))
) ;let

;; 带初始队列的 unfold
(let ((q0 (list-queue 8)))
  (let
    ((q
       (list-queue-unfold
         (lambda (x) (> x 3))
         (lambda (x) (* x 2))
         (lambda (x) (+ x 1))
         0
         q0)
       ) ;list-queue-unfold
     ) ;q
    (check (list-queue-list q) => '(0 2 4 6 8))
  ) ;let
) ;let

;; 立即终止的情况
(let
  ((q
     (list-queue-unfold
       (lambda (x) (> x 0))
       (lambda (x) x)
       (lambda (x) (+ x 1))
       1)
     ) ;list-queue-unfold
   ) ;q
  (check (list-queue-empty? q) => #t)
) ;let

(check-report)
