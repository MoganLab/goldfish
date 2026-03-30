(import (liii check)
        (liii iset)
) ;import

(check-set-mode! 'report-failed)

(define pos-seq (iota 20 100 3))
(define mixed-seq (iota 20 -10 3))
(define pos-set (list->iset pos-seq))
(define mixed-set (list->iset mixed-seq))

;;
;; isubset>
;; 返回集合中大于 k 的元素。
;;
;; 语法
;; ----
;; (isubset> iset k)
;;
(check (iset->list (isubset> pos-set 148)) => '(151 154 157))
(check (iset->list (isubset> mixed-set 38)) => '(41 44 47))

(check-report)
