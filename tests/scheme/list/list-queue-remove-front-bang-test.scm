(import (liii check)
        (srfi srfi-117)
) ;import

(check-set-mode! 'report-failed)

;; list-queue-remove-front!
;; 删除并返回队首元素。
;;
;; 语法
;; ----
;; (list-queue-remove-front! queue) -> any

(let ((queue (list-queue 1 2 3)))
  (check (list-queue-remove-front! queue) => 1)
  (check (list-queue-list queue) => '(2 3))
) ;let

(let ((queue (list-queue 42)))
  (check (list-queue-remove-front! queue) => 42)
  (check-true (list-queue-empty? queue))
) ;let

;; 错误处理测试
(check-catch 'out-of-range (list-queue-remove-front! (list-queue)))

(check-report)
