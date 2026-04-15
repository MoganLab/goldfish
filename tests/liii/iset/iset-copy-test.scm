(import (liii check) (liii iset))


(check-set-mode! 'report-failed)


(define pos-seq (iota 20 100 3))
(define pos-set (list->iset pos-seq))


;;
;; iset-copy
;; 复制一个集合。
;;
;; 语法
;; ----
;; (iset-copy iset)
;;
;; 返回值
;; -----
;; 返回包含相同元素的新 iset。
;;
(check-true (not (eqv? (iset-copy pos-set) pos-set))
) ;check-true
(check-true (iset=? (iset-copy pos-set) pos-set)
) ;check-true


(check-report)
