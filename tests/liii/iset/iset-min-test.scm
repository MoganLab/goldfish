(import (liii check)
        (liii iset)
) ;import

(check-set-mode! 'report-failed)

(define pos-seq (iota 20 100 3))
(define pos-set (list->iset pos-seq))

;;
;; iset-min
;; 返回集合中的最小元素。
;;
;; 语法
;; ----
;; (iset-min iset)
;;
;; 参数
;; ----
;; iset : iset
;; 要查询的集合。
;;
;; 返回值
;; -----
;; 返回集合中的最小整数，如果集合为空则返回 #f。
;;
(check (iset-min (iset 2 3 5 7 11)) => 2)
(check (iset-min (iset)) => #f)
(check (iset-min pos-set) => (car pos-seq))

(check-report)
