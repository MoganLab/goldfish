(import (liii check)
        (liii iset)
) ;import

(check-set-mode! 'report-failed)

(define pos-seq (iota 20 100 3))
(define neg-seq (iota 20 -100 3))
(define pos-set (list->iset pos-seq))
(define neg-set (list->iset neg-seq))

;;
;; iset-intersection
;; 返回多个集合的交集。
;;
;; 语法
;; ----
;; (iset-intersection iset1 iset2 ...)
;;
(check (iset->list (iset-intersection (iset 0 1 3 4) (iset 0 2 4)))
       => '(0 4)
) ;check
(check-true (iset-empty? (iset-intersection pos-set neg-set)))

(check-report)
