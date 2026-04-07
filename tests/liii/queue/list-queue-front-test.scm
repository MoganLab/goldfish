(import (liii check)
        (liii queue)
) ;import

(check-set-mode! 'report-failed)

;; list-queue-front 基本测试
(let ((q (list-queue 1 2 3)))
  (check (list-queue-front q) => 1)
) ;let

;; 单元素队列
(let ((q (list-queue 42)))
  (check (list-queue-front q) => 42)
) ;let

;; 不同类型
(let ((q (list-queue 'a "b" 3)))
  (check (list-queue-front q) => 'a)
) ;let

;; 前端添加后，front 变化
(let ((q (list-queue 2 3)))
  (list-queue-add-front! q 1)
  (check (list-queue-front q) => 1)
) ;let

;; 前端移除后，front 变化
(let ((q (list-queue 1 2 3)))
  (list-queue-remove-front! q)
  (check (list-queue-front q) => 2)
) ;let

;; 空队列报错
(check-catch 'wrong-type-arg (list-queue-front (list-queue)))

(check-report)
