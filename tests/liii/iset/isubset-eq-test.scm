(import (liii check) (liii iset))


(check-set-mode! 'report-failed)


(define pos-seq (iota 20 100 3))
(define pos-set (list->iset pos-seq))


;;
;; isubset=
;; 返回集合中等于 k 的元素（结果最多包含一个元素）。
;;
;; 语法
;; ----
;; (isubset= iset k)
;;
(check (iset->list (isubset= pos-set 90))
  =>
  '()
) ;check
(check (iset->list (isubset= pos-set 100))
  =>
  '(100)
) ;check


(check-report)
