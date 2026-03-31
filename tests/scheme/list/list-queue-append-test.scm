(import (liii check)
        (srfi srfi-117)
) ;import

(check-set-mode! 'report-failed)

;; list-queue-append
;; 以非破坏性方式拼接多个队列。
;;
;; 语法
;; ----
;; (list-queue-append queue ...) -> list-queue?

(let* ((queue-1 (list-queue 1 2))
       (queue-2 (list-queue 3 4))
       (queue-3 (list-queue-append queue-1 queue-2))
) ;let*
  (check (list-queue-list queue-3) => '(1 2 3 4))
  (check (list-queue-list queue-1) => '(1 2))
  (check (list-queue-list queue-2) => '(3 4))
) ;let*

;; 错误处理测试
(check-catch 'wrong-type-arg (list-queue-append (list-queue 1) 2))

(check-report)
