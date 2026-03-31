(import (liii check)
        (srfi srfi-117)
) ;import

(check-set-mode! 'report-failed)

;; list-queue-list
;; 返回队列当前底层列表。
;;
;; 语法
;; ----
;; (list-queue-list queue) -> list?

(check (list-queue-list (list-queue)) => '())
(check (list-queue-list (list-queue 1 2 3)) => '(1 2 3))

;; 错误处理测试
(check-catch 'wrong-type-arg (list-queue-list 1))

(check-report)
