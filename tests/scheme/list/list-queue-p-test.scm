(import (liii check)
        (srfi srfi-117)
) ;import

(check-set-mode! 'report-failed)

;; list-queue?
;; 判断对象是否为 list-queue。
;;
;; 语法
;; ----
;; (list-queue? obj) -> boolean?

(check-true (list-queue? (list-queue)))
(check-true (list-queue? (make-list-queue '(1 2 3))))
(check-false (list-queue? '(1 2 3)))
(check-false (list-queue? 1))
(check-false (list-queue? #f))

(check-report)
