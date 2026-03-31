(import (liii check)
        (srfi srfi-117)
) ;import

(check-set-mode! 'report-failed)

;; list-queue-unfold-right
;; 以后向构造的方式展开生成队列。
;;
;; 语法
;; ----
;; (list-queue-unfold-right stop? mapper successor seed)
;; (list-queue-unfold-right stop? mapper successor seed queue)

(let ((queue (list-queue-unfold-right (lambda (x) (> x 3))
                                      (lambda (x) (* x 2))
                                      (lambda (x) (+ x 1))
                                      0)))
  (check (list-queue-list queue) => '(6 4 2 0))
) ;let

(let ((tail (list-queue 8)))
  (let ((queue (list-queue-unfold-right (lambda (x) (> x 3))
                                        (lambda (x) (* x 2))
                                        (lambda (x) (+ x 1))
                                        0
                                        tail)))
    (check-true (eq? queue tail))
    (check (list-queue-list queue) => '(8 6 4 2 0))
  ) ;let
) ;let

(check-report)
