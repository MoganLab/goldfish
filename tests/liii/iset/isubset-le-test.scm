(import (liii check)
        (liii iset)
) ;import

(check-set-mode! 'report-failed)

(define pos-seq (iota 20 100 3))
(define mixed-seq (iota 20 -10 3))
(define pos-set (list->iset pos-seq))
(define mixed-set (list->iset mixed-seq))

;;
;; isubset<=
;; 返回集合中小于或等于 k 的元素。
;;
;; 语法
;; ----
;; (isubset<= iset k)
;;
(check (iset->list (isubset<= pos-set 109)) => '(100 103 106 109))
(check (iset->list (isubset<= mixed-set -4)) => '(-10 -7 -4))

(check-report)
