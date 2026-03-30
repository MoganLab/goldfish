(import (liii check)
        (liii iset)
) ;import

(check-set-mode! 'report-failed)

(define pos-seq (iota 20 100 3))
(define pos-set (list->iset pos-seq))
(define pos-set+ (iset-adjoin pos-set 9))

;;
;; iset<?
;; 检查第一个集合是否为第二个集合的真子集。
;;
;; 语法
;; ----
;; (iset<? iset1 iset2 ...)
;;
(check-true (iset<? (iset) pos-set))
(check-true (iset<? pos-set pos-set+))
(check-false (iset<? pos-set pos-set))

(check-report)
