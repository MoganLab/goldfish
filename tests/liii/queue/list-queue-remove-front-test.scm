(import (liii check) (liii queue))


(check-set-mode! 'report-failed)


;; list-queue-remove-front! 基本测试
(let ((q (list-queue 1 2 3)))
  (check (list-queue-remove-front! q)
    =>
    1
  ) ;check
  (check (list-queue-list q) => '(2 3))
) ;let


;; 单元素队列
(let ((q (list-queue 42)))
  (check (list-queue-remove-front! q)
    =>
    42
  ) ;check
  (check (list-queue-empty? q) => #t)
) ;let


;; 移除所有元素
(let ((q (list-queue 1 2 3)))
  (check (list-queue-remove-front! q)
    =>
    1
  ) ;check
  (check (list-queue-remove-front! q)
    =>
    2
  ) ;check
  (check (list-queue-remove-front! q)
    =>
    3
  ) ;check
  (check (list-queue-empty? q) => #t)
) ;let


;; 移除后可以继续添加
(let ((q (list-queue 1 2)))
  (list-queue-remove-front! q)
  (list-queue-add-front! q 0)
  (check (list-queue-list q) => '(0 2))
) ;let


;; 空队列报错
(check-catch 'wrong-type-arg
  (list-queue-remove-front! (list-queue))
) ;check-catch


(check-report)
