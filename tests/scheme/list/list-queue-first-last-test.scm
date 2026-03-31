(import (liii check)
        (only (scheme base) let-values)
        (srfi srfi-117)
) ;import

(check-set-mode! 'report-failed)

;; list-queue-first-last
;; 返回队列内部 first/last pair。
;;
;; 语法
;; ----
;; (list-queue-first-last queue) -> values

(let ((queue (list-queue)))
  (let-values (((first last) (list-queue-first-last queue)))
    (check first => '())
    (check last => '())
  ) ;let-values
) ;let

(let* ((pairs (list 'a 'b 'c))
       (queue (make-list-queue pairs (cddr pairs)))
) ;let*
  (let-values (((first last) (list-queue-first-last queue)))
    (check-true (eq? first pairs))
    (check-true (eq? last (cddr pairs)))
  ) ;let-values
) ;let*

;; 错误处理测试
(check-catch 'wrong-type-arg (list-queue-first-last 1))

(check-report)
