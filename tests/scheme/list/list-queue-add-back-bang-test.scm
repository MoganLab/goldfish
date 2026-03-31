(import (liii check)
        (srfi srfi-117)
) ;import

(check-set-mode! 'report-failed)

;; list-queue-add-back!
;; 将元素追加到队尾。
;;
;; 语法
;; ----
;; (list-queue-add-back! queue element)

(let ((queue (list-queue)))
  (list-queue-add-back! queue 1)
  (list-queue-add-back! queue 2)
  (list-queue-add-back! queue 3)
  (check (list-queue-list queue) => '(1 2 3))
  (check (list-queue-front queue) => 1)
  (check (list-queue-back queue) => 3)
) ;let

;; 错误处理测试
(check-catch 'wrong-type-arg (list-queue-add-back! 1 'a))

(check-report)
