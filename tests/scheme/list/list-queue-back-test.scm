(import (liii check)
        (srfi srfi-117)
) ;import

(check-set-mode! 'report-failed)

;; list-queue-back
;; 返回队尾元素。
;;
;; 语法
;; ----
;; (list-queue-back queue) -> any

(check (list-queue-back (list-queue 1 2 3)) => 3)

;; 错误处理测试
(check-catch 'out-of-range (list-queue-back (list-queue)))
(check-catch 'wrong-type-arg (list-queue-back 1))

(check-report)
