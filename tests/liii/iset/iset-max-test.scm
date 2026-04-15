(import (liii check) (liii iset))


(check-set-mode! 'report-failed)


(define pos-seq (iota 20 100 3))
(define pos-set (list->iset pos-seq))


;;
;; iset-max
;; 返回集合中的最大元素。
;;
;; 语法
;; ----
;; (iset-max iset)
;;
;; 参数
;; ----
;; iset : iset
;; 要查询的集合。
;;
;; 返回值
;; -----
;; 返回集合中的最大整数，如果集合为空则返回 #f。
;;
(check (iset-max (iset 2 3 5 7 11))
  =>
  11
) ;check
(check (iset-max (iset)) => #f)
(check (iset-max pos-set)
  =>
  (list-ref pos-seq
    (- (length pos-seq) 1)
  ) ;list-ref
) ;check


(check-report)
