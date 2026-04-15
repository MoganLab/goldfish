(import (liii check) (liii iset))


(check-set-mode! 'report-failed)


(define pos-seq (iota 20 100 3))
(define pos-set (list->iset pos-seq))


;;
;; iset-xor
;; 返回两个集合的对称差集。
;;
;; 语法
;; ----
;; (iset-xor iset1 iset2)
;;
(check (iset->list (iset-xor (iset 0 1 3) (iset 0 2 4))
       ) ;iset->list
  =>
  '(1 2 3 4)
) ;check
(check (iset->list (iset-xor pos-set pos-set))
  =>
  '()
) ;check


(check-report)
