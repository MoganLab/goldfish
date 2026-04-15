(import (liii check) (liii queue))


(check-set-mode! 'report-failed)


;; list-queue-append 基本测试
(let ((x (list-queue 1 2 3))
      (y (list-queue 4 5))
     ) ;
  (let ((z (list-queue-append x y)))
    (check (list-queue-list z)
      =>
      '(1 2 3 4 5)
    ) ;check
    ;; 原队列不受影响
    (check (list-queue-list x) => '(1 2 3))
    (check (list-queue-list y) => '(4 5))
  ) ;let
) ;let


;; 空队列
(let ((q1 (list-queue 1 2))
      (q2 (list-queue))
     ) ;
  (let ((z (list-queue-append q1 q2)))
    (check (list-queue-list z) => '(1 2))
  ) ;let
) ;let


;; 多个队列
(let ((q1 (list-queue 1))
      (q2 (list-queue 2))
      (q3 (list-queue 3))
     ) ;
  (let ((z (list-queue-append q1 q2 q3)))
    (check (list-queue-list z) => '(1 2 3))
  ) ;let
) ;let


;; 单个队列
(let ((q (list-queue 1 2 3)))
  (let ((z (list-queue-append q)))
    (check (list-queue-list z) => '(1 2 3))
  ) ;let
) ;let


(check-report)
