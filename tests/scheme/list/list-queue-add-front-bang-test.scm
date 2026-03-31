(import (liii check)
        (srfi srfi-117)
) ;import

(check-set-mode! 'report-failed)

;; list-queue-add-front!
;; 将元素插入到队首。
;;
;; 语法
;; ----
;; (list-queue-add-front! queue element)

(let ((queue (list-queue)))
  (list-queue-add-front! queue 2)
  (list-queue-add-front! queue 1)
  (check (list-queue-list queue) => '(1 2))
  (check (list-queue-front queue) => 1)
  (check (list-queue-back queue) => 2)
) ;let

;; 错误处理测试
(check-catch 'wrong-type-arg (list-queue-add-front! 1 'a))

(check-report)
