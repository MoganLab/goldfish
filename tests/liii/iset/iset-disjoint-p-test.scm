(import (liii check) (liii iset))


(check-set-mode! 'report-failed)


(define pos-seq (iota 20 100 3))
(define neg-seq (iota 20 -100 3))
(define pos-set (list->iset pos-seq))
(define neg-set (list->iset neg-seq))
(define dense-set
  (make-range-iset 0 49)
) ;define
(define sparse-set
  (list->iset (iota 20 -10000 1003))
) ;define


;;
;; iset-disjoint?
;; 检查两个集合是否不相交（没有共同元素）。
;;
;; 语法
;; ----
;; (iset-disjoint? iset1 iset2)
;;
;; 参数
;; ----
;; iset1, iset2 : iset
;; 要检查的集合。
;;
;; 返回值
;; -----
;; 如果两个集合没有共同元素，返回 #t；否则返回 #f。
;;
(check-true (iset-disjoint? (iset 1 3 5)
              (iset 0 2 4)
            ) ;iset-disjoint?
) ;check-true
(check-false (iset-disjoint? (iset 1 3 5)
               (iset 2 3 4)
             ) ;iset-disjoint?
) ;check-false
(check-true (iset-disjoint? pos-set neg-set)
) ;check-true
(check-false (iset-disjoint? dense-set sparse-set)
) ;check-false


(check-report)
