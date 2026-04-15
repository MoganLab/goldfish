(import (liii check) (liii iset))


(check-set-mode! 'report-failed)


(define pos-seq (iota 20 100 3))
(define pos-set (list->iset pos-seq))


;;
;; iset-size
;; 返回集合中元素的数量。
;;
;; 语法
;; ----
;; (iset-size iset)
;;
(check (iset-size (iset)) => 0)
(check (iset-size (iset 1 3 5)) => 3)
(check (iset-size pos-set)
  =>
  (length pos-seq)
) ;check


(check-report)
