(import (liii check)
        (srfi srfi-117)
) ;import

(check-set-mode! 'report-failed)

;; list-queue-unfold
;; 以前向构造的方式展开生成队列。
;;
;; 语法
;; ----
;; (list-queue-unfold stop? mapper successor seed)
;; (list-queue-unfold stop? mapper successor seed queue)

(let ((queue (list-queue-unfold (lambda (x) (> x 3))
                                (lambda (x) (* x 2))
                                (lambda (x) (+ x 1))
                                0)))
  (check (list-queue-list queue) => '(0 2 4 6))
) ;let

(let ((tail (list-queue 8)))
  (let ((queue (list-queue-unfold (lambda (x) (> x 3))
                                  (lambda (x) (* x 2))
                                  (lambda (x) (+ x 1))
                                  0
                                  tail)))
    (check-true (eq? queue tail))
    (check (list-queue-list queue) => '(0 2 4 6 8))
  ) ;let
) ;let

;; 错误处理测试
(check-catch 'wrong-type-arg
             (list-queue-unfold (lambda (x) #t)
                                (lambda (x) x)
                                (lambda (x) x)
                                0
                                1))

(check-report)
