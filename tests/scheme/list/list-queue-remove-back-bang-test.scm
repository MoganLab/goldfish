(import (liii check)
        (srfi srfi-117)
) ;import

(check-set-mode! 'report-failed)

;; list-queue-remove-back!
;; 删除并返回队尾元素。
;;
;; 语法
;; ----
;; (list-queue-remove-back! queue) -> any

(let ((queue (list-queue 1 2 3)))
  (check (list-queue-remove-back! queue) => 3)
  (check (list-queue-list queue) => '(1 2))
) ;let

(let ((queue (list-queue 42)))
  (check (list-queue-remove-back! queue) => 42)
  (check-true (list-queue-empty? queue))
) ;let

;; 错误处理测试
(check-catch 'out-of-range (list-queue-remove-back! (list-queue)))

(check-report)
