(import (liii check) (liii queue))


(check-set-mode! 'report-failed)


;; list-queue-map 基本测试
(let* ((q (list-queue 1 2 3))
       (r (list-queue-map (lambda (x) (* x 10)) q)
       ) ;r
      ) ;
  (check (list-queue-list r)
    =>
    '(10 20 30)
  ) ;check
  ;; 原队列不受影响
  (check (list-queue-list q) => '(1 2 3))
) ;let*


;; 空队列
(let ((q (list-queue-map (lambda (x) (* x 2))
           (list-queue)
         ) ;list-queue-map
      ) ;q
     ) ;
  (check (list-queue-empty? q) => #t)
) ;let


;; 单元素队列
(let ((r (list-queue-map (lambda (x) (+ x 1))
           (list-queue 42)
         ) ;list-queue-map
      ) ;r
     ) ;
  (check (list-queue-list r) => '(43))
) ;let


;; 返回不同类型
(let ((r (list-queue-map (lambda (x) (if (odd? x) 'odd 'even))
           (list-queue 1 2 3)
         ) ;list-queue-map
      ) ;r
     ) ;
  (check (list-queue-list r)
    =>
    '(odd even odd)
  ) ;check
) ;let


(check-report)
