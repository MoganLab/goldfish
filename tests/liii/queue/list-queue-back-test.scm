(import (liii check) (liii queue))


(check-set-mode! 'report-failed)


;; list-queue-back 基本测试
(let ((q (list-queue 1 2 3)))
  (check (list-queue-back q) => 3)
) ;let


;; 单元素队列
(let ((q (list-queue 42)))
  (check (list-queue-back q) => 42)
  (check (list-queue-front q) => 42)
) ;let


;; 后端添加后，back 变化
(let ((q (list-queue 1 2)))
  (list-queue-add-back! q 3)
  (check (list-queue-back q) => 3)
) ;let


;; 后端移除后，back 变化
(let ((q (list-queue 1 2 3)))
  (list-queue-remove-back! q)
  (check (list-queue-back q) => 2)
) ;let


;; 前端添加不影响 back
(let ((q (list-queue 2 3)))
  (list-queue-add-front! q 1)
  (check (list-queue-back q) => 3)
) ;let


;; 空队列报错
(check-catch 'wrong-type-arg
  (list-queue-back (list-queue))
) ;check-catch


(check-report)
