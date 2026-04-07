(import (liii check)
        (liii queue)
) ;import

(check-set-mode! 'report-failed)

;; list-queue-first-last 基本测试
(let ((q (list-queue 1 2 3)))
  (let-values (((first last) (list-queue-first-last q)))
    (check first => '(1 2 3))
    (check last => '(3))
  ) ;let-values
) ;let

;; 空队列
(let ((q (list-queue)))
  (let-values (((first last) (list-queue-first-last q)))
    (check first => '())
    (check last => '())
  ) ;let-values
) ;let

;; 单元素队列
(let ((q (list-queue 42)))
  (let-values (((first last) (list-queue-first-last q)))
    (check first => '(42))
    (check last => '(42))
    (check (eq? first last) => #t)
  ) ;let-values
) ;let

;; 验证 first 和 last 的关系
(let* ((lst '(1 2 3))
       (last-ptr (cddr lst))
       (q (make-list-queue lst last-ptr)))
  (let-values (((first last) (list-queue-first-last q)))
    (check (eq? first lst) => #t)
    (check (eq? last last-ptr) => #t)
  ) ;let-values
) ;let*

(check-report)
