(import (liii check)
        (srfi srfi-117)
) ;import

(check-set-mode! 'report-failed)

;; list-queue-for-each
;; 依次遍历队列中的元素。
;;
;; 语法
;; ----
;; (list-queue-for-each proc queue)

(let ((sum 0))
  (list-queue-for-each (lambda (x) (set! sum (+ sum x)))
                       (list-queue 10 20 30))
  (check sum => 60)
) ;let

(check-catch 'wrong-type-arg (list-queue-for-each (lambda (x) x) 1))

(check-report)
