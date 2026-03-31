(import (liii check)
        (srfi srfi-117)
) ;import

(check-set-mode! 'report-failed)

;; list-queue-empty?
;; 判断队列是否为空。
;;
;; 语法
;; ----
;; (list-queue-empty? queue) -> boolean?

(check-true (list-queue-empty? (list-queue)))
(check-false (list-queue-empty? (list-queue 1 2 3)))

;; 错误处理测试
(check-catch 'wrong-type-arg (list-queue-empty? 1))

(check-report)
