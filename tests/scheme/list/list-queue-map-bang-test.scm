(import (liii check)
        (srfi srfi-117)
) ;import

(check-set-mode! 'report-failed)

;; list-queue-map!
;; 原地修改队列中的每个元素。
;;
;; 语法
;; ----
;; (list-queue-map! proc queue)

(let ((queue (list-queue 1 2 3)))
  (list-queue-map! (lambda (x) (+ x 1)) queue)
  (check (list-queue-list queue) => '(2 3 4))
) ;let

(check-catch 'wrong-type-arg (list-queue-map! (lambda (x) x) 1))

(check-report)
