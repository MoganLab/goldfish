(import (liii check)
        (srfi srfi-117)
) ;import

(check-set-mode! 'report-failed)

;; list-queue-map
;; 返回应用映射后的新队列。
;;
;; 语法
;; ----
;; (list-queue-map proc queue) -> list-queue?

(let* ((queue (list-queue 1 2 3))
       (mapped (list-queue-map (lambda (x) (* x 10)) queue))
) ;let*
  (check (list-queue-list mapped) => '(10 20 30))
  (check (list-queue-list queue) => '(1 2 3))
) ;let*

(check-catch 'wrong-type-arg (list-queue-map (lambda (x) x) 1))

(check-report)
