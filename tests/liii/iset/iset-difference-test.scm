(import (liii check) (liii iset))


(check-set-mode! 'report-failed)


(define pos-seq (iota 20 100 3))
(define neg-seq (iota 20 -100 3))
(define pos-set (list->iset pos-seq))
(define neg-set (list->iset neg-seq))


;;
;; iset-difference
;; 返回第一个集合与其余集合的差集。
;;
;; 语法
;; ----
;; (iset-difference iset1 iset2 ...)
;;
(check (iset->list (iset-difference (iset 0 1 3 4)
                     (iset 0 2)
                     (iset 0 4)
                   ) ;iset-difference
       ) ;iset->list
  =>
  '(1 3)
) ;check
(check (iset->list (iset-difference pos-set neg-set)
       ) ;iset->list
  =>
  pos-seq
) ;check


(check-report)
