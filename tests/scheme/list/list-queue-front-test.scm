(import (liii check)
        (srfi srfi-117)
) ;import

(check-set-mode! 'report-failed)

;; list-queue-front
;; 返回队首元素。
;;
;; 语法
;; ----
;; (list-queue-front queue) -> any

(check (list-queue-front (list-queue 1 2 3)) => 1)

;; 错误处理测试
(check-catch 'out-of-range (list-queue-front (list-queue)))
(check-catch 'wrong-type-arg (list-queue-front 1))
(check-catch 'wrong-number-of-args (list-queue-front))

(check-report)
