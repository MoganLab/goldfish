(import (liii check)
        (liii iset)
) ;import

(check-set-mode! 'report-failed)

(define pos-seq (iota 20 100 3))
(define neg-seq (iota 20 -100 3))
(define pos-set (list->iset pos-seq))
(define neg-set (list->iset neg-seq))

;;
;; iset-union
;; 返回多个集合的并集。
;;
;; 语法
;; ----
;; (iset-union iset1 iset2 ...)
;;
(check (iset->list (iset-union (iset 0 1 3) (iset 0 2 4)))
       => '(0 1 2 3 4)
) ;check
(check (iset->list (iset-union pos-set neg-set))
       => (iset->list (list->iset (append pos-seq neg-seq)))
) ;check

(check-report)
